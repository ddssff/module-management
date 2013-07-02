{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Split
    ( DeclName(..)
    , splitModule
    , splitModuleDecls
    , defaultSymbolToModule
    ) where

import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Default (Default(def))
import Data.Foldable as Foldable (fold)
import Data.List as List (filter, intercalate, map, nub)
import Data.Map as Map (delete, elems, empty, filter, insert, insertWith, lookup, Map, mapWithKey)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>), mempty)
import Data.Sequence ((<|), (|>))
import Data.Set as Set (delete, difference, empty, filter, fold, insert, intersection, map, member, null, Set, singleton, toList, union, unions)
import Data.Set.Extra as Set (gFind, mapM)
import Language.Haskell.Exts (fromParseResult, ParseResult(ParseOk, ParseFailed))
import qualified Language.Haskell.Exts.Annotated as A (Decl, ImportDecl(..), ImportSpecList(..), Module(..), ModuleHead(ModuleHead), Name)
import Language.Haskell.Exts.Annotated.Simplify (sImportDecl, sImportSpec, sModuleName, sName)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrint, prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec, ImportDecl(..), ModuleName(..), Name(..))
import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldExports, foldHeader, foldImports, foldModule, ignore, ignore2, ModuleInfo)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Internal (doResult, modulePath, ModuleResult(..), MonadClean(getParams), Params(moduVerse, testMode), parseFileWithComments)
import Language.Haskell.Modules.Util.QIO (qLnPutStr, quietly)
import Language.Haskell.Modules.Util.Symbols (exports, imports, symbols)
import Prelude hiding (writeFile)
import System.FilePath ((<.>))

data DeclName
    = Exported S.Name -- Maybe because...?
    | Internal S.Name
    | ReExported S.Name
    | Instance
    deriving (Eq, Ord, Show)

isReExported :: DeclName -> Bool
isReExported (ReExported _) = True
isReExported _ = False

parseModule :: MonadClean m => S.ModuleName -> m ModuleInfo
parseModule name =
    do path <- modulePath name
       text <- liftIO $ readFile path
       (parsed, comments) <- parseFileWithComments path >>= return . fromParseResult
       return (parsed, text, comments)


-- | Split each of a module's declarations into a new module.  Update
-- the imports of all the modules in the moduVerse to reflect the split.
-- For example, if you have a module like
--
-- @
-- module Start (a, b, (.+.)) where
-- import
-- a = 1 + a
-- b = 2
-- c = 3
-- c' = 4
-- (.+.) = b + c
-- @
--
-- After running @splitModule defaultSymbolToModule@ the @Start@ module will be gone.  The
-- @a@ and @b@ symbols will be in new modules named @Start.A@ and
-- @Start.B@.  Because they were not exported by @Start@, the @c@ and
-- @c'@ symbols will both be in a new module named @Start.Internal.C@.
-- And the @.+.@ symbol will be in a module named
-- @Start.OtherSymbols@.  Note that this module needs to import new
-- @Start.A@ and @Start.Internal.C@ modules.
--
-- If we had imported and then re-exported a symbol in Start it would
-- go into a module named @Start.ReExported@.  Any instance declarations
-- would go into @Start.Instances@.
splitModule :: MonadClean m =>
               (S.ModuleName -> DeclName -> S.ModuleName) -- ^ Map declaration to new module name
            -> S.ModuleName
            -> m ()
splitModule symbolToModule old = parseModule old >>= splitModuleBy symbolToModule old

-- | Do splitModuleBy with the default symbol to module mapping (was splitModule)
splitModuleDecls :: MonadClean m => S.ModuleName -> m ()
splitModuleDecls old = parseModule old >>= \ m -> splitModuleBy defaultSymbolToModule old m

splitModuleBy :: MonadClean m =>
                 (S.ModuleName -> DeclName -> S.ModuleName)
              -> S.ModuleName               -- ^ Parent module name
              -> ModuleInfo -> m ()
splitModuleBy symbolToModule old m =
    do univ <- moduVerseCheck
       changes <- doSplit (symbolToModule old) univ m >>= return . collisionCheck univ
       setMapM_ doResult changes       -- Write the new modules
       setMapM_ doClean changes        -- Clean the new modules after all edits are finished
    where
      moduVerseCheck = getParams >>= return . fromMaybe (error "moduVerse not set, use modifyModuVerse") . moduVerse
      collisionCheck univ s =
          if not (Set.null illegal)
          then error ("One or more module to be created by splitModule already exists: " ++ show (Set.toList illegal))
          else s
          where
            illegal = Set.intersection univ (created s)
      created :: Set ModuleResult -> Set S.ModuleName
      created = setMapMaybe (\ x -> case x of Created m' _ -> Just m'; _ -> Nothing)
      -- Make sure this isn't trying to clobber a module that exists (other than 'old'.)
      doClean :: MonadClean m => ModuleResult -> m ()
      doClean (Created m' _) = doClean' m'
      doClean (Modified m' _) = doClean' m'
      doClean (Removed _) = return ()
      doClean (Unchanged _) = return ()
      doClean' m' =
          do flag <- getParams >>= return . not . testMode
             when flag (modulePath m' >>= cleanImports >> return ())

-- This returns a set of maybe because there may be instance
-- declarations, in which case we want an Instances module to
-- appear in newModuleNames below.
declared :: ModuleInfo -> Set (Maybe S.Name)
declared m = foldDecls (\ d _pref _s _suff r -> Set.union (symbols d) r) ignore2 m Set.empty

exported :: ModuleInfo -> Set (Maybe S.Name)
exported m =
    case hasExportList m of
      False -> declared m
      True -> union (foldExports ignore2 (\ e _pref _s _suff r -> Set.union (symbols e) r) ignore2 m Set.empty)
                    -- Any instances declared are exported regardless of the export list
                    (if member Nothing (declared m) then singleton Nothing else Set.empty)
    where
      hasExportList :: ModuleInfo -> Bool
      hasExportList (A.Module _ Nothing _ _ _, _, _) = False
      hasExportList (A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _, _, _) = False
      hasExportList _ = True

newModuleNames :: (DeclName -> S.ModuleName) -> ModuleInfo -> Set S.ModuleName
newModuleNames symbolToModule m =
    Set.map (symbolToModule . declName' m) (union (declared m) (exported m))

declName' :: ModuleInfo -> Maybe S.Name -> DeclName
declName' m =
    declName
    where
      declName :: Maybe S.Name -> DeclName
      declName name =
          case name of
            Nothing -> Instance
            Just name' ->
                if reExported name'
                then ReExported name'
                else if internal name'
                     then Internal name'
                     else Exported name'
      internal :: S.Name -> Bool
      internal name = member (Just name) (difference (declared m) (exported m))
      reExported :: S.Name -> Bool
      reExported name = member (Just name) (difference (exported m) (declared m))

-- | Create the set of module results implied by the split -
-- creations, removals, and modifications.  This includes the changes
-- to the imports in modules that imported the original module.
doSplit :: MonadClean m => (DeclName -> S.ModuleName) -> Set S.ModuleName -> ModuleInfo -> m (Set ModuleResult)
doSplit _ _ (A.XmlPage {}, _, _) = error "XmlPage"
doSplit _ _ (A.XmlHybrid {}, _, _) = error "XmlPage"
doSplit _ _ (A.Module _ _ _ _ [], _, _) = return Set.empty -- No declarations - nothing to split
doSplit _ _ (A.Module _ _ _ _ [_], _, _) = return Set.empty -- One declaration - nothing to split (but maybe we should anyway?)
doSplit _ _ (A.Module _ Nothing _ _ _, _, _) = throw $ userError $ "splitModule: no explicit header"
doSplit symbolToModule univ m@(A.Module _ (Just (A.ModuleHead _ moduleName _ _)) _ _ _, _, _) =
    do qLnPutStr ("Splitting " ++ show moduleName)
       importChanges <- Set.mapM (updateImports m (sModuleName moduleName) symbolToModule) (Set.delete old univ)
       return $ unions [ -- The changes required to existing imports
                         importChanges
                         -- Compute the result of splitting the parent module
                       , Set.map newModule moduleNames
                         -- Did the parent module disappear, or was it replaced?
                       , if member old moduleNames then Set.empty else singleton (Removed old) ]
    where
      moduleNames = newModuleNames symbolToModule m
      -- The name of the module to be split
      old = sModuleName moduleName

      -- Build a map from module name to the list of declarations that
      -- will be in that module.  All of these declarations used to be
      -- in moduleName.
      moduleDeclMap :: Map S.ModuleName [(A.Decl SrcSpanInfo, String)]
      moduleDeclMap = foldDecls (\ d pref s suff r -> Set.fold (\ sym mp -> insertWith (++) (symbolToModule (declName' m sym)) [(d, pref <> s <> suff)] mp) r (symbols d)) ignore2 m Map.empty

      -- Build a new module given its name and the list of
      -- declarations it should contain.
      newModule :: S.ModuleName -> ModuleResult
      newModule name'@(S.ModuleName modName) =
          (if member name' univ then Modified else Created) name' $
          case Map.lookup name' moduleDeclMap of
            Nothing ->
                -- Build a module that re-exports a symbol
                Foldable.fold (foldHeader echo2 echo (\ _n pref _ suff r -> r |> pref <> modName <> suff) echo m mempty) <>
                Foldable.fold (foldExports echo2 ignore ignore2 m mempty) <>
                doSeps (Foldable.fold (foldExports ignore2
                                          (\ e pref s suff r -> if setAny isReExported  (Set.map (declName' m) (symbols e)) then r |> [(pref, s <> suff)] else r)
                                          (\ s r -> r |> [("", s)]) m mempty)) <>
                Foldable.fold (foldImports (\ _i pref s suff r -> r |> pref <> s <> suff) m mempty)
            Just modDecls ->
                -- Change the module name in the header
                Foldable.fold (foldHeader echo2 echo (\ _n pref _ suff r -> r |> pref <> modName <> suff) echo m mempty) <>
                -- If the module has an export list use its outline
                (let mh = let (A.Module _ x _ _ _, _, _) = m in x
                     me = maybe Nothing (\ h -> let (A.ModuleHead _ _ _ x) = h in x) mh in
                 maybe "\n    ( " (\ _ -> Foldable.fold $ foldExports (<|) ignore ignore2 m mempty) me <>
                 intercalate "\n    , " (nub (List.map (prettyPrintWithMode defaultMode) (newExports modDecls))) <> "\n" <>
                 maybe "    ) where\n" (\ _ -> Foldable.fold $ foldExports ignore2 ignore (<|) m mempty) me) <>
                -- The prefix of the imports section
                fromMaybe "" (foldImports (\ _i pref _ _ r -> maybe (Just pref) Just r) m Nothing) <>
                unlines (List.map (prettyPrintWithMode defaultMode) (elems (newImports modDecls))) <> "\n" <>
                -- Grab the old imports
                fromMaybe "" (foldImports (\ _i pref s suff r -> Just (maybe (s <> suff) (\ l -> l <> pref <> s <> suff) r)) m Nothing) <>
                -- fromMaybe "" (foldDecls (\ _d pref _ _ r -> maybe (Just pref) Just r) ignore2 m text Nothing) <>
                concatMap snd (reverse modDecls)
              where
                -- Build export specs of the symbols created by each declaration.
                newExports :: [(A.Decl SrcSpanInfo, String)] -> [S.ExportSpec]
                newExports xs = nub (concatMap (exports . fst) xs)

                newImports :: [(A.Decl SrcSpanInfo, String)] -> Map S.ModuleName S.ImportDecl
                newImports xs =
                    mapWithKey toImportDecl (Map.delete name'
                                             (Map.filter (\ pairs ->
                                                              let declared' = justs (Set.unions (List.map (symbols . fst) pairs)) in
                                                              not (Set.null (Set.intersection declared' (referenced xs)))) moduleDeclMap))
                -- In this module, we need to import any module that declares a symbol
                -- referenced here.
                referenced :: [(A.Decl SrcSpanInfo, String)] -> Set S.Name
                referenced xs = Set.map sName (gFind xs :: Set (A.Name SrcSpanInfo))

-- Re-construct a separated list
doSeps :: [(String, String)] -> String
doSeps [] = ""
doSeps ((_, hd) : tl) = hd <> concatMap (\ (a, b) -> a <> b) tl

-- | Update the imports to reflect the changed module names in symbolToModule.
updateImports :: MonadClean m => ModuleInfo -> S.ModuleName -> (DeclName -> S.ModuleName) -> S.ModuleName -> m ModuleResult
updateImports m old symbolToModule name =
    do path <- modulePath name
       quietly $ qLnPutStr $ "updateImports " ++ show name
       text' <- liftIO $ readFile path
       parsed <- parseFileWithComments path
       case parsed of
         ParseOk (m', comments') ->
             let text'' = Foldable.fold (foldModule echo2 echo echo echo echo2 echo echo2
                                                    (\ i pref s suff r -> r |> pref <> updateImportDecl s i <> suff)
                                                    echo echo2 (m', text', comments') mempty) in
             return $ if text' /= text'' then Modified name text'' else Unchanged name
         ParseFailed _ _ -> error $ "Parse error in " ++ show name
    where
      updateImportDecl :: String -> A.ImportDecl SrcSpanInfo -> String
      updateImportDecl s i =
          if sModuleName (A.importModule i) == old
          then intercalate "\n" (List.map prettyPrint (updateImportSpecs i (A.importSpecs i)))
          else s

      updateImportSpecs :: A.ImportDecl SrcSpanInfo -> Maybe (A.ImportSpecList SrcSpanInfo) -> [S.ImportDecl]
      -- No spec list, import all the split modules
      updateImportSpecs i Nothing = List.map (\ x -> (sImportDecl i) {S.importModule = x}) (Map.elems moduleMap)
      -- If flag is True this is a "hiding" import
      updateImportSpecs i (Just (A.ImportSpecList _ flag specs)) =
          concatMap (\ spec -> let xs = mapMaybe (\ sym -> Map.lookup (declName' m sym) moduleMap) (toList (symbols spec)) in
                               List.map (\ x -> (sImportDecl i) {S.importModule = x, S.importSpecs = Just (flag, [sImportSpec spec])}) xs) specs

      moduleMap = symbolToModuleMap symbolToModule m

symbolToModuleMap :: (DeclName -> S.ModuleName) -> ModuleInfo -> Map DeclName S.ModuleName
symbolToModuleMap symbolToModule m =
    mp'
    where
      mp' = foldExports ignore2 (\ e _ _ _ r -> Set.fold f r (symbols e)) ignore2 m mp
      mp = foldDecls (\ d _ _ _ r -> Set.fold f r (symbols d)) ignore2 m Map.empty
      f sym mp'' =
          Map.insert
            (declName' m sym)
            (symbolToModule (declName' m sym))
            mp''

-- | What module should this symbol be moved to?
defaultSymbolToModule :: S.ModuleName -- ^ Parent module name
                      -> DeclName     -- ^ Declared symbol
                      -> S.ModuleName
defaultSymbolToModule (S.ModuleName parentModuleName) name =
    S.ModuleName (parentModuleName <.> case name of
                                         Instance -> "Instances"
                                         ReExported _ -> "ReExported"
                                         Internal x -> "Internal" <.> f x
                                         Exported x -> f x)
    where
      f (S.Symbol s) = g s
      f (S.Ident s) = g s
      -- Any symbol that starts with a letter is converted to a module name
      -- by capitalizing and keeping the remaining alphaNum characters.
      g (c : s) | isAlpha c = toUpper c : List.filter isAlphaNum s
      g _ = "OtherSymbols"

-- | Build an import of the symbols created by a declaration.
toImportDecl :: S.ModuleName -> [(A.Decl SrcSpanInfo, String)] -> S.ImportDecl
toImportDecl (S.ModuleName modName) decls =
    S.ImportDecl {S.importLoc = def,
                  S.importModule = S.ModuleName modName,
                  S.importQualified = False,
                  S.importSrc = False,
                  S.importPkg = Nothing,
                  S.importAs = Nothing,
                  S.importSpecs = Just (False, nub (concatMap (imports . fst) decls))}

justs :: Ord a => Set (Maybe a) -> Set a
justs = Set.fold (\ mx s -> maybe s (`Set.insert` s) mx) Set.empty

setAny :: Ord a => (a -> Bool) -> Set a -> Bool
setAny f s = not (Set.null (Set.filter f s))

setMapMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
setMapMaybe p s = Set.fold f Set.empty s
    where f x s' = maybe s' (\ y -> Set.insert y s') (p x)

setMapM_ :: (Monad m, Ord b) => (a -> m b) -> Set a -> m ()
setMapM_ f s = do _ <- Set.mapM f s
                  return ()
