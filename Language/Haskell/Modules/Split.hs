{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Split
    ( splitModule
    , splitModuleDecls
    , defaultSymbolToModule
    ) where

import Control.Exception (throw)
import Control.Monad as List (mapM, mapM_)
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Default (Default(def))
import Data.Foldable as Foldable (fold)
import Data.List as List (filter, group, intercalate, map, nub, sort)
import Data.Map as Map (delete, elems, empty, filter, fromSet, insertWith, lookup, Map, mapWithKey)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mempty)
import Data.Sequence ((<|), (|>))
import Data.Set as Set (empty, filter, fold, insert, intersection, map, member, null, Set, singleton, toList, union, unions)
import Data.Set.Extra as Set (gFind)
import qualified Language.Haskell.Exts.Annotated as A (Decl, ImportDecl(..), ImportSpecList(..), Module(..), ModuleHead(ModuleHead), Name)
import Language.Haskell.Exts.Annotated.Simplify (sExportSpec, sImportDecl, sImportSpec, sModule, sModuleName, sName)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrint, prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(..), ImportDecl(..), Module(..), ModuleName(..), Name(..))
import Language.Haskell.Modules.Common (doResult, ModuleResult(..), reportResult)
import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldExports, foldHeader, foldImports, foldModule, ignore, ignore2)
import Language.Haskell.Modules.Imports (cleanResults)
import Language.Haskell.Modules.ModuVerse (findModule, getNames, ModuleInfo(..), moduleName, parseModule)
import Language.Haskell.Modules.Params (MonadClean(getParams), Params(extraImports))
import Language.Haskell.Modules.SourceDirs (modulePathBase, APath(..), pathKey)
import Language.Haskell.Modules.Util.QIO (qLnPutStr, quietly)
import Language.Haskell.Modules.Util.Symbols (exports, imports, symbols, members)
import Prelude hiding (writeFile)
import System.FilePath ((<.>))

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
-- After running @splitModuleDecls "Start.hs"@ the @Start@ module will
-- be gone.  The @a@ and @b@ symbols will be in new modules named
-- @Start.A@ and @Start.B@.  Because they were not exported by
-- @Start@, the @c@ and @c'@ symbols will both be in a new module
-- named @Start.Internal.C@.  And the @.+.@ symbol will be in a module
-- named @Start.OtherSymbols@.  Note that this module needs to import
-- new @Start.A@ and @Start.Internal.C@ modules.
--
-- If we had imported and then re-exported a symbol in Start it would
-- go into a module named @Start.ReExported@.  Any instance declarations
-- would go into @Start.Instances@.
splitModule :: MonadClean m =>
               (Maybe S.Name -> S.ModuleName)
               -- ^ Map declaration to new module name.   The name @Nothing@
               -- is used for instance declarations.
            -> FilePath
            -> m [ModuleResult]
splitModule symToModule path =
    do info <- pathKey (APath path) >>= parseModule
       splitModuleBy symToModule info

-- | Do splitModuleBy with the default symbol to module mapping (was splitModule)
splitModuleDecls :: MonadClean m => FilePath -> m [ModuleResult]
splitModuleDecls path =
    do info <- pathKey (APath path) >>= parseModule
       splitModuleBy (defaultSymbolToModule info) info

splitModuleBy :: MonadClean m => (Maybe S.Name -> S.ModuleName) -> ModuleInfo -> m [ModuleResult]
splitModuleBy _ (ModuleInfo (A.XmlPage {}) _ _ _) = error "XmlPage"
splitModuleBy _ (ModuleInfo (A.XmlHybrid {}) _ _ _) = error "XmlPage"
splitModuleBy _ m@(ModuleInfo (A.Module _ _ _ _ []) _ _ key) = return [Unchanged (moduleName m) key] -- No declarations - nothing to split
splitModuleBy _ m@(ModuleInfo (A.Module _ _ _ _ [_]) _ _ key) = return [Unchanged (moduleName m) key] -- One declaration - nothing to split (but maybe we should anyway?)
splitModuleBy _ (ModuleInfo (A.Module _ Nothing _ _ _) _ _ _) = throw $ userError $ "splitModule: no explicit header"
splitModuleBy symToModule inInfo =
    do qLnPutStr ("Splitting module " ++ prettyPrint (moduleName inInfo))
       quietly $
         do eiMap <- getParams >>= return . extraImports
            -- The name of the module to be split
            let inName = moduleName inInfo
            allNames <- getNames >>= return . Set.union outNames
            changes <- List.mapM (doModule symToModule eiMap inInfo inName outNames) (toList allNames)
            -- No good reason to use sets here
            -- changes <- doSplit symToModule info >>= return . collisionCheck univ
            changes' <- List.mapM doResult changes           -- Write the new modules
            List.mapM_ (\ x -> qLnPutStr ("splitModule: " ++ reportResult x)) changes'
            -- Clean the new modules after all edits are finished
            cleanResults changes'
    where

{-    collisionCheck univ s =
          if (not $ Set.null $ Set.intersection univ $ Set.filter isCreated s)
          then error ("One or more module to be created by splitModule already exists: " ++ show (Set.toList illegal))
          else s -}

      outNames = Set.map symToModule (union (declared inInfo) (exported inInfo))

doModule :: MonadClean m =>
            (Maybe S.Name -> S.ModuleName)
         -> Map S.ModuleName (Set S.ImportDecl)
         -> ModuleInfo -> S.ModuleName
         -> Set S.ModuleName -> S.ModuleName -> m ModuleResult
doModule symToModule eiMap inInfo inName outNames thisName =
    case () of
      _ | member thisName outNames ->
            findModule thisName >>= \ thisInfo ->
            return $ if thisName == inName
                     then ToBeModified thisName (key_ inInfo) newModule
                     else case thisInfo of
                            Just (ModuleInfo {key_ = key}) ->
                                error $ "splitModule: output module already exists: " ++ show key
                            _ -> ToBeCreated thisName newModule
        | thisName == inName -> return (ToBeRemoved thisName (key_ inInfo))
        | True ->
            pathKey (modulePathBase "hs" thisName) >>= parseModule >>= \ oldInfo@(ModuleInfo _ oldText _ _) ->
            let newText = updateImports oldInfo in
            return $ if newText /= oldText then ToBeModified thisName (key_ oldInfo) newText else Unchanged thisName (key_ oldInfo)
    where
      -- Build a new module given its name and the list of
      -- declarations it should contain.
      newModule :: String
      newModule = newHeader <> newExports <> newImports <> newDecls

      -- Change the module name in the header
      newHeader =
        Foldable.fold (foldHeader echo2 echo (\ _n pref _ suff r -> r |> pref <> prettyPrint thisName <> suff) echo inInfo mempty)

      newExports =
        -- If the module has an export list use its outline
        maybe "\n    ( " (\ _ -> Foldable.fold $ foldExports (<|) ignore ignore2 inInfo mempty) (moduleExports inInfo) <>

        case Map.lookup thisName moduleDeclMap of
           Nothing ->
               doSeps (Foldable.fold (foldExports ignore2
                                       (\ e pref s suff r ->
                                            if setAny isReExported (Set.map (declClass inInfo) (symbols e))
                                            then r |> [(pref, s <> suff)]
                                            else r)
                                       (\ s r -> r |> [("", s)]) inInfo mempty))
           Just _ ->
              intercalate sep (nub (List.map (prettyPrintWithMode defaultMode) newExports')) <> "\n" <>
              maybe "    ) where\n" (\ _ -> Foldable.fold $ foldExports ignore2 ignore (<|) inInfo mempty) (moduleExports inInfo)
          where
            -- Build export specs of the symbols created by each declaration.
            newExports' :: [S.ExportSpec]
            newExports' = nub (concatMap (exports . fst) modDecls)

            -- Re-construct a separated list
            doSeps :: [(String, String)] -> String
            doSeps [] = ""
            doSeps ((_, hd) : tl) = hd <> concatMap (\ (a, b) -> a <> b) tl

            sep = exportSep "\n    , " inInfo

      newImports =
          case (oldImportText, newImports'') of
            ([], "") -> "\n"
            ([], s) -> s
            ((pref, s, suff) : more, i) -> pref <> i <> s <> suff ++ concatMap (\ (pref', s', suff') -> pref' <> s' <> suff') more
          where
            oldImportText = foldImports (\ _ pref s suff r -> r ++ [(pref, s, suff)]) inInfo []
            newImports'' =
                case Map.lookup thisName moduleDeclMap of
                  Nothing -> ""
                  Just _ -> unlines (List.map (prettyPrintWithMode defaultMode) (newImports' ++ instanceImports thisName eiMap))
            -- Import all the referenced symbols that are declared in
            -- the original module and referenced in the new module.
            newImports' :: [S.ImportDecl]
            newImports' =
                elems $
                mapWithKey toImportDecl $
                Map.delete thisName $
                Map.filter imported moduleDeclMap
                where
                  imported pairs =
                      let declared' = justs (Set.unions (List.map (symbols . fst) pairs ++
                                                         List.map (members . fst) pairs)) in
                      not (Set.null (Set.intersection declared' referenced))

      newDecls = concatMap snd modDecls

      modDecls :: [(A.Decl SrcSpanInfo, String)]
      modDecls = fromMaybe [] (Map.lookup thisName moduleDeclMap)

      moduleExports (ModuleInfo (A.Module _ Nothing _ _ _) _ _ _) = Nothing
      moduleExports (ModuleInfo (A.Module _ (Just (A.ModuleHead _ _ _ x)) _ _ _) _ _ _) = x
      moduleExports (ModuleInfo _ _ _ _) = error "Unsupported module type"

      -- Update the imports to reflect the changed module names in symToModule.
      -- Update re-exports of the split module.
      updateImports :: ModuleInfo -> String
      updateImports oldInfo =
          Foldable.fold (foldModule echo2 echo echo echo echo2
                                    (\ e pref s suff r -> r |> pref <> fixExport s (sExportSpec e) <> suff) echo2
                                    (\ i pref s suff r -> r |> pref <> updateImportDecl s i <> suff)
                                    echo echo2 oldInfo mempty)
          where
            -- If we see the input module re-exported, replace with all the output modules
            fixExport :: String -> S.ExportSpec -> String
            fixExport _ (S.EModuleContents m) | m == inName = intercalate sep (List.map (prettyPrint . S.EModuleContents) (toList outNames))
            fixExport s _ = s

            sep = exportSep "\n    , " oldInfo

      -- In this module, we need to import any module that declares a symbol
      -- referenced here.
      referenced :: Set S.Name
      referenced = Set.map sName (gFind modDecls :: Set (A.Name SrcSpanInfo))

      -- Build a map from module name to the list of declarations that
      -- will be in that module.  All of these declarations used to be
      -- in moduleName.
      moduleDeclMap :: Map S.ModuleName [(A.Decl SrcSpanInfo, String)]
      moduleDeclMap = foldDecls (\ d pref s suff r -> Set.fold (\ sym mp -> insertWith (\ a b -> b ++ a) (symToModule sym) [(d, pref <> s <> suff)] mp) r (symbols d)) ignore2 inInfo Map.empty

      updateImportDecl :: String -> A.ImportDecl SrcSpanInfo -> String
      updateImportDecl s i =
          if sModuleName (A.importModule i) == inName
          then intercalate "\n" (List.map prettyPrint (updateImportSpecs i (A.importSpecs i) ++ instanceImports thisName eiMap))
          else s

      updateImportSpecs :: A.ImportDecl SrcSpanInfo -> Maybe (A.ImportSpecList SrcSpanInfo) -> [S.ImportDecl]
      -- No spec list, import all the split modules
      updateImportSpecs i Nothing = List.map (\ x -> (sImportDecl i) {S.importModule = x}) (Set.toList moduleNames) -- (Map.elems moduleMap)
      -- If flag is True this is a "hiding" import
      updateImportSpecs i (Just (A.ImportSpecList _ flag specs)) =
          concatMap (\ spec -> let xs = List.map symToModule (toList (symbols spec)) in
                               List.map (\ x -> (sImportDecl i) {S.importModule = x, S.importSpecs = Just (flag, [sImportSpec spec])}) xs) specs

      moduleNames :: Set S.ModuleName
      moduleNames =
          s
          where
            s = foldExports ignore2 (\ e _ _ _ r -> Set.fold (Set.insert . symToModule) r (symbols e)) ignore2 inInfo s'
            s' = foldDecls (\ d _ _ _ r -> Set.fold (Set.insert . symToModule) r (symbols d)) ignore2 inInfo Set.empty

-- | Combine the suffix of each export with the prefix of the following
-- export to make a list of all the separators.  Discards the first
-- prefix and the last suffix, if all the remaining separators are
-- equal return it, otherwise return the default argument.
exportSep :: String -> ModuleInfo -> String
exportSep defsep info =
    case seps of
      [] -> defsep
      (_ : xs) -> case (group . sort) xs of
                    [[x]] -> x -- We could choose the most common one here
                    _ -> defsep
    where
      seps = foldModule ignore2 ignore ignore ignore ignore2
                        (\ _ pref _ suff r -> case r of
                                                [] -> [suff]
                                                (suff' : xs) -> suff : (pref ++ suff') : xs)
                        ignore2 ignore ignore ignore2 info []

-- | Return a list of the names declared in this module, Nothing
-- denotes one or more instances.
declared :: ModuleInfo -> Set (Maybe S.Name)
declared m = foldDecls (\ d _pref _s _suff r -> Set.union (symbols d) r) ignore2 m Set.empty

-- | Return a list of the names expored by this module, Nothing
-- denotes instances.
exported :: ModuleInfo -> Set (Maybe S.Name)
exported m =
    case hasExportList m of
      False -> declared m
      True -> union (foldExports ignore2 (\ e _pref _s _suff r -> Set.union (symbols e) r) ignore2 m Set.empty)
                    -- Any instances declared are exported regardless of the export list
                    (if member Nothing (declared m) then singleton Nothing else Set.empty)
    where
      hasExportList :: ModuleInfo -> Bool
      hasExportList (ModuleInfo (A.Module _ Nothing _ _ _) _ _ _) = False
      hasExportList (ModuleInfo (A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _) _ _ _) = False
      hasExportList _ = True

instanceImports :: S.ModuleName -> Map S.ModuleName (Set S.ImportDecl) -> [S.ImportDecl]
instanceImports name eiMap = maybe [] toList (Map.lookup name eiMap)

data DeclClass
    = Exported S.Name
    | Internal S.Name
    | ReExported S.Name
    | Instance
    | Unknown S.Name
    deriving (Eq, Ord, Show)

-- | Classify the symbols in a module.
declClass :: ModuleInfo -> Maybe S.Name -> DeclClass
declClass info@(ModuleInfo m _ _ _) mName =
    unknown mName $ Map.lookup mName mp
    where
      unknown :: Maybe S.Name -> Maybe DeclClass -> DeclClass
      unknown _ (Just x) = x
      unknown Nothing _ = error "declClass Nothing"
      unknown (Just x) _ = Unknown x
      mp = Map.fromSet declClass' (union declaredSymbols exportedSymbols)
      declClass' Nothing = Instance
      declClass' x@(Just name) =
          if member x exportedSymbols
          then if member x declaredSymbols
               then Exported name
               else ReExported name -- Exported but not declared - must come from an import
          else Internal name        -- Not exported
      declaredSymbols = foldDecls (\ d _pref _s _suff r -> Set.union (symbols d) r) ignore2 info Set.empty
      exportedSymbols =
        case hasExportList (sModule m) of
          False -> declaredSymbols
          True -> union (foldExports ignore2 (\ e _pref _s _suff r -> Set.union (symbols e) r) ignore2 info Set.empty)
                        -- Any instances declared are exported regardless of the export list
                        (if member Nothing declaredSymbols then singleton Nothing else Set.empty)
        where
          hasExportList :: S.Module -> Bool
          hasExportList (S.Module _ _ _ _ Nothing _ _) = False
          hasExportList _ = True

isReExported :: DeclClass -> Bool
isReExported (ReExported _) = True
isReExported _ = False

-- | This can be used to build function parameter of splitModule, it
-- determines which module should a symbol be moved to.
defaultSymbolToModule :: ModuleInfo    -- ^ Parent module name
                      -> Maybe S.Name
                      -> S.ModuleName
defaultSymbolToModule info name =
    S.ModuleName (parentModuleName <.>
                             case declClass info name of
                               Instance -> "Instances"
                               ReExported _ -> "ReExported"
                               Internal x -> "Internal" <.> f x
                               Unknown x -> "Unknown" <.> f x
                               Exported x -> f x)
    where
      S.ModuleName parentModuleName = moduleName info
      f (S.Symbol s) = g s
      f (S.Ident s) = g s
      -- Any symbol that starts with a letter is converted to a module name
      -- by capitalizing and keeping the remaining alphaNum characters.
      g (c : s) | isAlpha c = toUpper c : List.filter isAlphaNum s
      g _ = "OtherSymbols"

-- Default is "import Main ()"
instance Default S.ImportDecl where
    def = S.ImportDecl {S.importLoc = SrcLoc "<unknown>.hs" 1 1,
                        S.importModule = S.ModuleName "Main",
                        S.importQualified = False,
                        S.importSrc = False,
                        S.importPkg = Nothing,
                        S.importAs = Nothing,
                        S.importSpecs = Just (False, [])}

-- | Build an import of the symbols created by a declaration.
toImportDecl :: S.ModuleName -> [(A.Decl SrcSpanInfo, String)] -> S.ImportDecl
toImportDecl (S.ModuleName modName) decls =
    def { S.importModule = S.ModuleName modName
        , S.importSpecs = Just (False, nub (concatMap (imports . fst) decls)) }

justs :: Ord a => Set (Maybe a) -> Set a
justs = Set.fold (\ mx s -> maybe s (`Set.insert` s) mx) Set.empty

setAny :: Ord a => (a -> Bool) -> Set a -> Bool
setAny f s = not (Set.null (Set.filter f s))

-- setMapMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
-- setMapMaybe p s = Set.fold f Set.empty s
--     where f x s' = maybe s' (\ y -> Set.insert y s') (p x)
