{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-missing-signatures -fno-warn-unused-imports #-}
module Language.Haskell.Modules.Split
    ( splitModule
    , splitModuleBy
    , newModule
    ) where

import Debug.Trace
import Control.Exception (throw)
import Control.Lens ((.=), use)
import Control.Monad as List (mapM {-, mapM_-})
import Control.Monad.State (get, put, runState, State)
import Data.Default (Default(def))
import Data.Foldable as Foldable (fold)
import Data.List as List (group, intercalate, map, nub, sort)
import Data.Map as Map (delete, elems, empty, filter, insertWith, keys, lookup, Map, mapWithKey)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (<|), (|>))
import Data.Set as Set (empty, filter, fromList, insert, intersection, map, member, null, Set, singleton, toList, union)
import qualified Data.Set as Set (fold)
import Data.Set.Extra as Set (gFind)
import qualified Language.Haskell.Exts.Annotated as A (Decl(InstDecl), ExportSpec(..), ExportSpecList, ImportDecl(..), ImportSpec(..), ImportSpecList(..), Module(..), ModuleHead(ModuleHead), Name)
import Language.Haskell.Exts.Annotated.Simplify (sExportSpec, sImportDecl, sImportSpec, sModule, sModuleName, sName)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrint, prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(..), ImportDecl(..), Module(..), ModuleName(..), Name(..))
import Language.Haskell.Modules.Common (doResult, ModuleResult(..) {-, reportResult-})
import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldExports, foldHeader, foldImports, foldModule, ignore, ignore2, ModuleInfo(..))
import Language.Haskell.Modules.FoldM ((|$>), echoM, echo2M, foldDeclsM, foldExportsM, foldHeaderM, foldImportsM, foldModuleM, ignoreM, ignore2M)
import Language.Haskell.Modules.Imports (cleanResults)
import Language.Haskell.Modules.ModuVerse (buildSymbolMap, buildDeclMap, extraImports, CleanMode, findModule,
                                           modulesNew, moduleName, ModuVerse, moveFunction, Params, parseModule, symbolMap)
import Language.Haskell.Modules.SourceDirs (modulePathBase, APath(..), pathKey)
import Language.Haskell.Modules.Symbols (exports, FoldDeclared, foldDeclared, imports, symbolsDeclaredBy, members)
--import Language.Haskell.Modules.Util.QIO (qLnPutStr {-, quietly-})
import Prelude hiding (writeFile)

-- flatten :: Ord a => Set (Set a) -> Set a
-- flatten = foldl' Set.union mempty

-- | Split the declarations of the module in the input file into new
-- modules as specified by the 'toModule' function, which maps
-- symbol name's to module names.  It is permissable for the output
-- function to map one or more symbols to the original module.  The
-- modules will be written into files whose names are constructed from
-- the module name in the usual way, but with a prefix taken from the
-- first element of the list of directories in the 'SourceDirs' list.
-- This list is just @["."]@ by default.
splitModule :: ModuVerse m =>
               (S.ModuleName -> A.Decl SrcSpanInfo -> S.ModuleName)
            -> FilePath
            -- ^ The file containing the input module.
            -> m [ModuleResult]
splitModule toModule path =
    do info <- pathKey (APath ({-trace ("splitModule: " ++ show path)-} path)) >>= parseModule
       splitModuleBy toModule info

{-
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
splitModuleDecls :: ModuVerse m =>
                    CleanMode
                 -> FilePath
                 -- ^ The file containing the input module.
                 -> m [ModuleResult]
splitModuleDecls mode path =
    do info <- pathKey (APath path) >>= parseModule
       splitModuleBy mode (defaultToModule info) info

-- | This can be used to build the function parameter of 'splitModule',
-- it determines which module should a declaration be moved to.
defaultToModule :: ModuleInfo    -- ^ Parent module name
                -> ToModuleArg
defaultToModule info (Just name) (A decl) =
    S.ModuleName (parentModuleName <.>
                             case declClass info (Right decl) name of
                               Just (ReExported _ _) -> "ReExported"
                               Just (Internal _ x) -> "Internal" <.> f x
                               Just (Exported _ x) -> f x
                               Just (Instance _) -> "Instances" -- this can't happen
                               Nothing -> "Error")
    where
      S.ModuleName parentModuleName = moduleName info
      f (S.Symbol s) = g s
      f (S.Ident s) = g s
      -- Any symbol that starts with a letter is converted to a module name
      -- by capitalizing and keeping the remaining alphaNum characters.
      g (c : s) | isAlpha c = toUpper c : List.filter isAlphaNum s
      g _ = "OtherSymbols"
-}

-- | Do splitModuleBy with the default symbol to module mapping (was splitModule)
splitModuleBy :: ModuVerse m =>
                 (S.ModuleName -> A.Decl SrcSpanInfo -> S.ModuleName)
              -> ModuleInfo
              -- ^ The parsed input module.
              -> m [ModuleResult]
splitModuleBy _ (ModuleInfo (A.XmlPage {}) _ _ _ _) = error "XmlPage"
splitModuleBy _ (ModuleInfo (A.XmlHybrid {}) _ _ _ _) = error "XmlPage"
splitModuleBy _ m@(ModuleInfo (A.Module _ _ _ _ []) _ _ key _) = return [Unchanged (moduleName m) key] -- No declarations - nothing to split
splitModuleBy _ m@(ModuleInfo (A.Module _ _ _ _ [_]) _ _ key _) = return [Unchanged (moduleName m) key] -- One declaration - nothing to split (but maybe we should anyway?)
splitModuleBy _ (ModuleInfo (A.Module _ Nothing _ _ _) _ _ _ _) = throw $ userError $ "splitModule: no explicit header"
splitModuleBy toModule inInfo@(ModuleInfo (A.Module _ (Just (A.ModuleHead _ inName _ _)) _ _ _) _ _ _ _) =
    do -- qLnPutStr ("Splitting module " ++ prettyPrint (moduleName inInfo))
       moveFunction .= toModule
       buildSymbolMap
       buildDeclMap
       -- quietly $
       -- The name of the module to be split
       -- let inName = moduleName inInfo
       let outModuleNames :: Set S.ModuleName
           outModuleNames =
              foldDecls (\d  _ _ _ s -> Set.insert (toModule (sModuleName inName) d) s) (\_ s -> s) inInfo mempty
       allModuleNames <- Set.union outModuleNames <$> (Set.fromList . keys <$> use modulesNew)
       changes <- List.mapM (doModule inInfo (sModuleName inName) outModuleNames)
                            (toList allModuleNames)
       -- Now we have to clean the import lists of the new
       -- modules, which means writing the files and doing an IO
       -- operation (i.e. running ghc.)  Could we do this in a
       -- private place and then reload and return the result?
       changes' <- List.mapM doResult changes           -- Write the new modules
       -- List.mapM_ (\ x -> qLnPutStr ("splitModule: " ++ reportResult x)) changes'
       -- Clean the new modules after all edits are finished
       cleanResults changes'

-- | Perform updates a module.
doModule :: ModuVerse m =>
            ModuleInfo
         -> S.ModuleName
         -> Set S.ModuleName
         -> S.ModuleName
         -> m ModuleResult
doModule inInfo inName outNames thisName = do
  thisInfo <- findModule thisName
  case (member thisName outNames, thisName == inName) of
    (True, True) ->
        ToBeModified thisName (key_ inInfo) <$> newModule inInfo thisName
    (True, False)
        | isJust thisInfo ->
            error $ "splitModule: output module already exists: " ++ show (key_ (fromJust thisInfo))
    (True, False) ->
        ToBeCreated thisName <$> newModule inInfo thisName
    (False, True) ->
        pure $ ToBeRemoved thisName (key_ inInfo)
    _ -> do
      oldInfo@(ModuleInfo _ oldText _ _ _) <- pathKey (modulePathBase "hs" thisName) >>= parseModule
      newText <- updateImports oldInfo inInfo inName thisName outNames
      pure $ if newText /= oldText then ToBeModified thisName (key_ oldInfo) newText else Unchanged thisName (key_ oldInfo)

-- | Build the text of a new module given its name and the list of
-- declarations it should contain.
newModule :: ModuVerse m => ModuleInfo -> S.ModuleName -> m String
newModule inInfo thisName = do
  h <- newHeader inInfo thisName
  e <- newExports inInfo thisName
  i <- newImports inInfo thisName
  d <- newDecls inInfo thisName
  return $ h <> e <> i <> d

-- Change the module name in the header
newHeader :: ModuVerse m => ModuleInfo -> S.ModuleName -> m String
newHeader inInfo thisName =
        pure (Foldable.fold (foldHeader echo2 echo (\ _n pref _ suff r -> r |> pref <> prettyPrint thisName <> suff) echo inInfo mempty))

newExports :: ModuVerse m => ModuleInfo -> S.ModuleName -> m String
newExports inInfo thisName =
    use moveFunction >>= \toModule ->
    pure $
        -- If the module has an export list use its outline
        maybe "\n    ( " (\ _ -> Foldable.fold $ foldExports (<|) ignore ignore2 inInfo mempty) (moduleExports inInfo) <>

        case Map.lookup thisName (moduleDeclMap toModule inInfo) of
           Nothing ->
               doSeps (Foldable.fold (foldExports ignore2
                                       (\ e pref s suff r ->
                                            if setAny isReExported (declClasses inInfo (Left e))
                                            then r |> [(pref, s <> suff)]
                                            else r)
                                       (\ s r -> r |> [("", s)]) inInfo mempty))
           Just _ ->
              intercalate sep (Prelude.map (prettyPrintWithMode defaultMode) (newExports' toModule)) <> "\n" <>
              maybe "    ) where\n" (\ _ -> Foldable.fold $ foldExports ignore2 ignore (<|) inInfo mempty) (moduleExports inInfo)
          where
            -- Build export specs of the symbols created by each declaration.
            newExports' :: (S.ModuleName -> A.Decl SrcSpanInfo -> S.ModuleName) -> [S.ExportSpec]
            newExports' toModule = nub $ concat $ Prelude.map (exports . fst) (modDecls toModule inInfo thisName)

            -- Re-construct a separated list
            doSeps :: [(String, String)] -> String
            doSeps [] = ""
            doSeps ((_, hd) : tl) = hd <> concatMap (\ (a, b) -> a <> b) tl
            doSeps _ = error "doSeps"

            sep = exportSep "\n    , " inInfo

newImports :: ModuVerse m => ModuleInfo -> S.ModuleName -> m String
newImports inInfo thisName = do
  ni <- newImports''
  case (oldImportText, ni) of
    ([], "") -> pure "\n"
    ([], s) -> pure s
    ((pref, s, suff) : more, i) -> pure (pref <> i <> s <> suff ++ concatMap (\ (pref', s', suff') -> pref' <> s' <> suff') more)
    _ -> error "newImports"
    where
            oldImportText :: [(String, String, String)]
            oldImportText = foldImports (\ _ pref s suff r -> r ++ [(pref, s, suff)]) inInfo []
            newImports'' :: ModuVerse m => m String
            newImports'' =
              use moveFunction >>= \toModule ->
              case Map.lookup thisName (moduleDeclMap toModule inInfo) of
                Nothing -> pure ""
                Just _ -> unlines . List.map (prettyPrintWithMode defaultMode) <$> ((<>) <$> pure (newImports' toModule) <*> instanceImports thisName)
            -- Import all the referenced symbols that are declared in
            -- the original module and referenced in the new module.
            newImports' :: (S.ModuleName -> A.Decl SrcSpanInfo -> S.ModuleName) -> [S.ImportDecl]
            newImports' toModule =
                elems $
                mapWithKey toImportDecl $
                Map.delete thisName $
                Map.filter imported (moduleDeclMap toModule inInfo)
                where
                  imported :: [(A.Decl SrcSpanInfo, String)] -> Bool
                  imported pairs =
                      let declared' :: [S.Name]
                          declared' = concat (Prelude.map (symbolsDeclaredBy . fst) pairs <> Prelude.map (members . fst) pairs) in
                      not (Set.null (Set.intersection (Set.fromList declared') (referenced toModule inInfo thisName)))

-- | Build the text of the declaration section of the new module
newDecls :: ModuVerse m => ModuleInfo -> S.ModuleName -> m String
newDecls inInfo thisName =
    use moveFunction >>= \toModule ->
    pure $ concatMap snd (modDecls toModule inInfo thisName)

modDecls :: (S.ModuleName -> A.Decl SrcSpanInfo -> S.ModuleName) -> ModuleInfo -> S.ModuleName -> [(A.Decl SrcSpanInfo, String)]
modDecls toModule inInfo thisName = fromMaybe [] (Map.lookup thisName (moduleDeclMap toModule inInfo))

moduleExports :: ModuleInfo -> Maybe (A.ExportSpecList SrcSpanInfo)
moduleExports (ModuleInfo (A.Module _ Nothing _ _ _) _ _ _ _) = Nothing
moduleExports (ModuleInfo (A.Module _ (Just (A.ModuleHead _ _ _ x)) _ _ _) _ _ _ _) = x
moduleExports (ModuleInfo _ _ _ _ _) = error "Unsupported module type"

-- Update the imports to reflect the changed module names in toModule.
-- Update re-exports of the split module.
updateImports :: ModuVerse m => ModuleInfo -> ModuleInfo -> S.ModuleName -> S.ModuleName -> Set S.ModuleName -> m String
updateImports oldInfo inInfo inName thisName outNames = do
  Foldable.fold <$> (foldModuleM echo2M echoM echoM echoM echo2M
                        (\ e pref s suff r -> pure r |$> pure pref |$> pure (fixExport s (sExportSpec e)) |$> pure suff) echo2M
                        (\ i pref s suff r -> pure r |$> pure pref |$> updateImportDecl inInfo inName thisName s i |$> pure suff)
                        echoM echo2M oldInfo mempty)
          where
            -- If we see the input module re-exported, replace with all the output modules
            fixExport :: String -> S.ExportSpec -> String
            fixExport _ (S.EModuleContents m) | m == inName = intercalate sep (List.map (prettyPrint . S.EModuleContents) (toList outNames))
            fixExport s _ = s

            sep = exportSep "\n    , " oldInfo

-- In this module, we need to import any module that declares a symbol
-- referenced here.
referenced :: (S.ModuleName -> A.Decl SrcSpanInfo -> S.ModuleName) -> ModuleInfo -> S.ModuleName -> Set S.Name
referenced toModule inInfo thisName = Set.map sName (gFind (modDecls toModule inInfo thisName) :: Set (A.Name SrcSpanInfo))

-- Build a map from module name to the list of declarations that
-- will be in that module.  All of these declarations used to be
-- in moduleName.  Result is a list so we can preserve the order.
moduleDeclMap :: (S.ModuleName -> A.Decl SrcSpanInfo -> S.ModuleName) -> ModuleInfo -> Map S.ModuleName [(A.Decl SrcSpanInfo, String)]
moduleDeclMap toModule inInfo =
    foldDecls (\ d pref s suff r ->
                   insertWith (flip (<>)) (toModule (name_ inInfo) d) [(d, pref <> s <> suff)] r) ignore2 inInfo Map.empty

updateImportDecl :: ModuVerse m => ModuleInfo -> S.ModuleName -> S.ModuleName -> String -> A.ImportDecl SrcSpanInfo -> m String
updateImportDecl inInfo inName thisName s i =
    if sModuleName (A.importModule i) == inName
    then (intercalate "\n" . List.map prettyPrint) <$> ((<>) <$> updateImportSpecs inInfo i <*> instanceImports thisName)
    else pure s

-- | Build the imports for the new module.
updateImportSpecs :: forall m. ModuVerse m =>
                     ModuleInfo
                  -> A.ImportDecl SrcSpanInfo
                  -> m [S.ImportDecl]
-- No spec list, import all the split modules
updateImportSpecs inInfo i@(A.ImportDecl {A.importSpecs = Nothing}) = do
  (List.map (\ x -> (sImportDecl i) {S.importModule = x}) . Set.toList) <$> moduleNames inInfo
updateImportSpecs _inInfo i@(A.ImportDecl _l _modName _q _src _safe _pkg _as (Just (A.ImportSpecList _ True _specs))) =
    pure [sImportDecl i]    -- If flag is True this is a "hiding" import.  Will deal with this later.
updateImportSpecs _inInfo i@(A.ImportDecl {A.importModule = modName, A.importSpecs = Just (A.ImportSpecList _ flag specs)}) =
    -- We have explicit imports from the module.  Look at the symbols
    -- and find out what module they are in now.
    concat <$> mapM updateImportSpec specs
    where
      updateImportSpec :: A.ImportSpec SrcSpanInfo -> m [S.ImportDecl]
      updateImportSpec spec = do
        toModule <- use moveFunction
        findDecl (sModuleName modName) spec >>=
                 maybe (pure [sImportDecl i])
                       (\decl -> let x = toModule (sModuleName modName) decl in
                                 pure [(sImportDecl i) {S.importModule = x, S.importSpecs = Just (flag, [sImportSpec spec])}])

findDecl :: ModuVerse m => FoldDeclared a => S.ModuleName -> a -> m (Maybe (A.Decl SrcSpanInfo))
findDecl modName x = do
  let syms = foldDeclared (:) [] x
  decls <- (nub . catMaybes) <$> mapM (\sym -> Map.lookup (modName, sym) <$> use symbolMap) syms
  case decls of
    [decl] -> pure $ Just decl
    [] -> pure Nothing
    decls -> error ("Multiple declarations found for symbols " ++ show syms ++ " in " ++ prettyPrint modName ++ ": " ++ show (List.map prettyPrint decls))

moduleNames :: ModuVerse m => ModuleInfo -> m (Set S.ModuleName)
moduleNames inInfo = pure mempty
{-
  s
    where
      s = foldExports ignore2 (\ e _ _ _ r -> Set.fold (\sym -> Set.insert (toModule (S.ModuleName "fixme") (C e))) r (Set.fromList (symbolsDeclaredBy e))) ignore2 inInfo s'
      s' = foldDecls (\ d _ _ _ r -> Set.fold (\sym -> Set.insert (toModule (S.ModuleName "fixme") (A d))) r (Set.fromList (symbolsDeclaredBy d))) ignore2 inInfo Set.empty
-}

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
      _ -> error "exportSep"
    where
      seps = foldModule ignore2 ignore ignore ignore ignore2
                        (\ _ pref _ suff r -> case r of
                                                [] -> [suff]
                                                (suff' : xs) -> suff : (pref ++ suff') : xs
                                                _ -> error "exportSep")
                        ignore2 ignore ignore ignore2 info []

-- | Return a list of this module's declarations
declared :: ModuleInfo -> [A.Decl SrcSpanInfo]
declared m = foldDecls (\d _pref _s _suff r -> d : r) ignore2 m []

instanceImports :: ModuVerse m => S.ModuleName -> m [S.ImportDecl]
instanceImports name = do
  eiMap <- use extraImports
  pure $ maybe [] id (Map.lookup name eiMap)

data SymbolClass
    = Exported (Either (A.ExportSpec SrcSpanInfo) (A.Decl SrcSpanInfo)) S.Name
    | Internal (Either (A.ExportSpec SrcSpanInfo) (A.Decl SrcSpanInfo)) S.Name
    | ReExported (Either (A.ExportSpec SrcSpanInfo) (A.Decl SrcSpanInfo)) S.Name
    | Instance (A.Decl SrcSpanInfo)
    deriving (Eq, Ord, Show)

-- | Classify the symbols of a Decl or ExportSpec.
declClasses :: ModuleInfo -> Either (A.ExportSpec SrcSpanInfo) (A.Decl SrcSpanInfo) -> Set SymbolClass -- Map S.Name SymbolClass?
declClasses (ModuleInfo _ _ _ _ _) (Right decl@(A.InstDecl {})) = singleton (Instance decl)
declClasses info@(ModuleInfo m _ _ _ _) eed =
    Set.map symbolClass (Set.fromList (either symbolsDeclaredBy symbolsDeclaredBy eed))
    where
      -- mp = Map.fromSet symbolClass (union (symbolsDeclaredBy decl) exportedSymbols)
      symbolClass :: S.Name -> SymbolClass
      symbolClass name =
          case (member name moduleSymbols, member name exportedSymbols) of
            (True, True) -> Exported eed name    -- Declared and exported - must be a local declaration
            (False, True) -> ReExported eed name -- Not declared, but exported - must come from an import
            (_, False) -> Internal eed name      -- Not exported, but presumably declared
      -- All the symbols declared in this module
      moduleSymbols :: Set S.Name
      moduleSymbols = foldDecls (\ d _pref _s _suff r -> union (Set.fromList (symbolsDeclaredBy d)) r) ignore2 info Set.empty
      -- All the symbols exported from this module
      exportedSymbols :: Set S.Name
      exportedSymbols =
        case hasExportList (sModule m) of
          False -> moduleSymbols
          True -> union (foldExports ignore2 (\ e _pref _s _suff r -> Set.union (Set.fromList (symbolsDeclaredBy e)) r) ignore2 info Set.empty)
                        -- Any instances declared are exported regardless of the export list
                        ({-if member Nothing moduleSymbols then singleton Nothing else-} Set.empty)
        where
          hasExportList :: S.Module -> Bool
          hasExportList (S.Module _ _ _ _ Nothing _ _) = False
          hasExportList _ = True

isReExported :: SymbolClass -> Bool
isReExported (ReExported _ _) = True
isReExported _ = False

-- Default is "import Main ()"
instance Default S.ImportDecl where
    def = S.ImportDecl {S.importLoc = SrcLoc "<unknown>.hs" 1 1,
                        S.importModule = S.ModuleName "Main",
                        S.importQualified = False,
                        S.importSrc = False,
                        S.importSafe = False, -- ?
                        S.importPkg = Nothing,
                        S.importAs = Nothing,
                        S.importSpecs = Just (False, [])}

-- | Build an import of the symbols created by a declaration.
toImportDecl :: S.ModuleName -> [(A.Decl SrcSpanInfo, String)] -> S.ImportDecl
toImportDecl (S.ModuleName modName) decls =
    def { S.importModule = S.ModuleName modName
        , S.importSpecs = Just (False, concat (Prelude.map (imports . fst) decls)) }

-- justs :: Ord a => Set (Maybe a) -> Set a
-- justs = Set.fold (\ mx s -> maybe s (`Set.insert` s) mx) Set.empty

setAny :: Ord a => (a -> Bool) -> Set a -> Bool
setAny f s = not (Set.null (Set.filter f s))

-- setMapMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
-- setMapMaybe p s = Set.fold f Set.empty s
--     where f x s' = maybe s' (\ y -> Set.insert y s') (p x)
