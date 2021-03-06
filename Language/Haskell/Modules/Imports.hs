{-# LANGUAGE CPP, PackageImports, ScopedTypeVariables, StandaloneDeriving, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Imports
    ( cleanImports
    , cleanResults
    ) where

import Control.Exception.Lifted as IO (bracket, catch, throw)
import Control.Lens (use)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (find, groupBy, intercalate, nub, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Sequence ((|>))
import Data.Set as Set (empty, fromList, map, member, Set, singleton, toList, union, unions)
#if MIN_VERSION_haskell_src_exts(1,18,0)
import qualified Language.Haskell.Exts.Syntax as A (Decl(DerivDecl), ImportDecl(..), ImportSpec(..), ImportSpecList(ImportSpecList), InstHead(..), Module(..), ModuleHead(..), ModuleName(..), QName(..), Type(..))
import qualified Language.Haskell.Exts.Syntax as A (InstRule(..))
#else
import Language.Haskell.Exts.Annotated.Simplify as S (sImportDecl, sImportSpec, sModuleName, sName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(DerivDecl), ImportDecl(..), ImportSpec(..), ImportSpecList(ImportSpecList), InstHead(..), Module(..), ModuleHead(..), ModuleName(..), QName(..), Type(..))
import qualified Language.Haskell.Exts.Annotated.Syntax as A (InstRule(..))
#endif
import Language.Haskell.Exts.Extension (Extension(..))
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ImportDecl(importAnn, importModule, importSpecs), ModuleName(..), Name(..))
import Language.Haskell.Modules.Common (ModuleResult(..))
import Language.Haskell.Modules.FoldM (foldDeclsM, foldExportsM, foldHeaderM, foldImportsM, ModuleInfo(..))
import Language.Haskell.Modules.ModuVerse (cleanMode, getExtensions, loadModule, ModuVerse, parseModule,
                                           markForDelete, hsFlags, removeEmptyImports, scratchDir, CleanMode(..))
import Language.Haskell.Modules.SourceDirs (modifyHsSourceDirs, modKey, AHsDir(..), ModKey(..), ModKey(_modKey), SourceDirs(getHsSourceDirs, putHsSourceDirs))
import Language.Haskell.Modules.SrcLoc (srcLoc)
import Language.Haskell.Modules.Symbols (symbolsDeclaredBy)
import Language.Haskell.Modules.Util.DryIO (replaceFile, tildeBackup)
--import Language.Haskell.Modules.Util.QIO (qLnPutStr, quietly)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode, showCommandForUser)

{-
-- | This is designed to be called from the postConf script of your
-- Setup file, it cleans up the imports of all the source files in the
-- package.

cleanBuildImports :: LocalBuildInfo -> IO ()
cleanBuildImports lbi =
    mapM (toFilePath srcDirs) (maybe [] exposedModules (library (localPkgDescr lbi))) >>= \ libPaths ->
    runModuVerseT (Distribution.Simple.LocalBuildInfo.scratchDir lbi) $ mapM_ clean (libPaths ++ exePaths)
    where
      clean path = cleanImports path >>= liftIO . putStrLn . either show (\ text -> path ++ ": " ++ maybe "no changes" (\ _ -> " updated") text)
      exePaths = map modulePath (executables (localPkgDescr lbi))
      srcDirs = case (maybe [] hsSourceDirs . fmap libBuildInfo . library . localPkgDescr $ lbi) of
                  [] -> ["."]
                  xs -> xs
      toFilePath :: [FilePath] -> D.ModuleName -> IO FilePath
      toFilePath [] m = error $ "Missing module: " ++ intercalate "." (D.components m)
      toFilePath (dir : dirs) m =
          let path = (dir </> intercalate "/" (D.components m) <.> "hs") in
          doesFileExist path >>= \ exists ->
          if exists then return path else toFilePath dirs m
-}

-- | Clean up the imports of a source file.  This means:
--
--    * All import lines get an explict list of symbols
--
--    * Imports of unused symbols are removed
--
--    * Imports of modules whose symbol list becomse empty are
--      removed, unless the 'removeEmptyImports' flag is set to
--      @False@.  However, imports that started out with an empty
--      import list @()@ are retained
--
--    * Repeated imports are merged
--
--    * Imports are alphabetized by module name
--
--    * Imported symbols are alphabetized by symbol name
--
--    * Imported constructors and field accessors are alphabetized
cleanImports :: ModuVerse m => [FilePath] -> m [ModuleResult]
cleanImports paths =
    do keys <- mapM (modKey . AHsDir) paths >>= return . fromList
       dumpImports keys
       mapM (\ key -> loadModule key >>= checkImports) (toList keys)

-- | Do import cleaning in response to the values returned by the
-- split and merge operations.  Module import lists are cleaned if the
-- module is modified or created.
cleanResults :: ModuVerse m => [ModuleResult] -> m [ModuleResult]
cleanResults results = do
  mode <- use cleanMode
  case mode of
    NoClean -> return results
    DoClean -> dump >> clean
    where
      dump =
          mapM (\ x -> case x of
                         JustRemoved _ -> return Nothing
                         Unchanged _ -> return Nothing
                         JustModified key -> return (Just key)
                         JustCreated key -> {-findModule key >>= return . fmap key_-} pure (Just key)
                         _ -> error $ "cleanResults - unexpected ModuleResult " ++ show x) results >>=
          dumpImports . fromList . catMaybes
      clean =
          mapM (\ x -> case x of
                         JustRemoved _ -> return x
                         Unchanged _ -> return x
                         JustModified key -> doModule key
                         JustCreated key -> doModule key >>= return . toCreated
                         _ -> error $ "cleanResults - unexpected ModuleResult " ++ show x) results
      -- The cleaning may have turned a Created result into Modified,
      -- turn it back into Created.
      toCreated (JustModified key) = JustCreated key
      toCreated x@(JustCreated {}) = x
      toCreated _ = error "toCreated"
      -- Update the cached version of the now modified module and then
      -- clean its import list.
      doModule key =
          do info <- loadModule key
             checkImports info

-- | Run ghc with -ddump-minimal-imports and capture the resulting .imports file.
dumpImports :: ModuVerse m => Set ModKey -> m ()
dumpImports keys =
    do scratch <- use scratchDir
       liftIO $ createDirectoryIfMissing True scratch
       let cmd = "ghc"
       args <- use hsFlags
       dirs <- getHsSourceDirs
       exts <- getExtensions
       let args' = args ++
                   ["--make", "-c", "-ddump-minimal-imports", "-outputdir", scratch, "-i" ++ intercalate ":" dirs] ++
                   concatMap ppExtension exts ++
                   Set.toList (Set.map _modKey keys)
       (code, _out, err) <- liftIO $ readProcessWithExitCode cmd args' ""
       case code of
         ExitSuccess -> {-quietly (qLnPutStr (showCommandForUser cmd args' ++ " -> Ok")) >>-} return ()
         ExitFailure _ -> error ("dumpImports: compile failed\n " ++ showCommandForUser cmd args' ++ " ->\n" ++ err)

    where
    ppExtension (EnableExtension x) = ["-X"++ show x]
    ppExtension _ = []

-- | Parse the import list generated by GHC, parse the original source
-- file, and if all goes well insert the new imports into the old
-- source file.  We also need to modify the imports of any names
-- that are types that appear in standalone instance derivations so
-- their members are imported too.
checkImports :: ModuVerse m => ModuleInfo -> m ModuleResult
checkImports info@(ModuleInfo (A.Module _ mh _ imports _) _ _ _ _) =
    do
       scratch <- use scratchDir
       let name = maybe "Main" (\ (A.ModuleHead _ (A.ModuleName _ s) _ _) -> s) mh
       let importsPath = scratch </> name ++ ".imports"

       -- The .imports file will appear in the real current directory,
       -- ignore the source dir path.  This may change in future
       -- versions of GHC, see http://ghc.haskell.org/trac/ghc/ticket/7957
       markForDelete importsPath
       (ModuleInfo newImports _ _ _ _) <-
           withDot $
               (loadModule (ModKey importsPath (S.ModuleName () name))
                  `IO.catch` (\ (e :: IOError) -> liftIO (getCurrentDirectory >>= \ here ->
                                                          throw . userError $ here ++ ": " ++ show e)))
       updateSource info newImports extraImports
    where
      extraImports = filter isHiddenImport imports
      isHiddenImport (A.ImportDecl {A.importSpecs = Just (A.ImportSpecList _ True _)}) = True
      isHiddenImport _ = False
checkImports _ = error "Unsupported module type"

withDot :: ModuVerse m => m a -> m a
withDot a =
    bracket (getHsSourceDirs)
            (modifyHsSourceDirs . const)
            (\ _ -> putHsSourceDirs ["."] >> a)

-- | If all the parsing went well and the new imports differ from the
-- old, update the source file with the new imports.
updateSource :: ModuVerse m => ModuleInfo -> A.Module SrcSpanInfo -> [A.ImportDecl SrcSpanInfo] -> m ModuleResult
updateSource m@(ModuleInfo (A.Module _ _ _ oldImports _) _ _ key _) (A.Module _ _ _ newImports _) extraImports =
    do remove <- use removeEmptyImports
       newImports' <- replaceImports (fixNewImports remove m oldImports (newImports ++ extraImports)) m
       maybe ({-qLnPutStr ("cleanImports: no changes to " ++ show key) >>-} return (Unchanged key))
             (\ text' ->
                  -- qLnPutStr ("cleanImports: modifying " ++ show key) >>
                  replaceFile tildeBackup (_modKey key) text' >>
                  return (JustModified key))
             newImports'
updateSource _ _ _ = error "updateSource"

-- | Compare the old and new import sets and if they differ clip out
-- the imports from the sourceText and insert the new ones.
replaceImports :: ModuVerse m => [A.ImportDecl SrcSpanInfo] -> ModuleInfo -> m (Maybe String)
replaceImports newImports m = do
  oldPretty <- fold <$> (foldImportsM (\ _ pref s suff r -> pure $ r |> (pref <> s <> suff)) m mempty)
        -- Surround newPretty with the same prefix and suffix as oldPretty
  importPref <- foldImportsM (\ _ pref _ _ r -> pure $ maybe (Just pref) Just r) m Nothing
  importSuff <- foldImportsM (\ _ _ _ suff _ -> pure suff) m mempty
  let newPretty = fromMaybe "" importPref <>
                  intercalate "\n" (Prelude.map (prettyPrintWithMode (defaultMode {layout = PPInLine})) newImports) <>
                  importSuff
  case oldPretty == newPretty of
    True -> pure Nothing
    False -> do
      h <- foldHeaderM (\ s r -> pure $ r |> s)
                      (\ _ pref s suff r -> pure $ r |> (pref <> s <> suff))
                      (\ _ pref s suff r -> pure $ r |> pref <> s <> suff)
                      (\ _ pref s suff r -> pure $ r |> pref <> s <> suff) m mempty
      e <- foldExportsM (\ s r -> pure $ r |> s)
                       (\ _ pref s suff r -> pure $ r |> pref <> s <> suff)
                       (\ s r -> pure $ r |> s) m mempty
      d <- foldDeclsM  (\ _ pref s suff r -> pure $ r |> pref <> s <> suff)
                      (\ r s -> pure $ s |> r) m mempty
      pure $ Just (fold h ++ fold e ++ newPretty <> fold d)

-- | Final touch-ups - sort and merge similar imports.
fixNewImports :: Bool         -- ^ If true, imports that turn into empty lists will be removed
              -> ModuleInfo
              -> [A.ImportDecl SrcSpanInfo]
              -> [A.ImportDecl SrcSpanInfo]
              -> [A.ImportDecl SrcSpanInfo]
fixNewImports remove m oldImports imports =
    filter importPred $ Prelude.map expandSDTypes $ Prelude.map mergeDecls $ groupBy (\ a b -> importMergable a b == EQ) $ sortBy importMergable imports
    where
      -- mergeDecls :: [ImportDecl] -> ImportDecl
      mergeDecls [] = error "mergeDecls"
      mergeDecls xs@(x : _) = x {A.importSpecs = mergeSpecLists (catMaybes (Prelude.map A.importSpecs xs))}
          where
            -- Merge a list of specs for the same module
            mergeSpecLists :: [A.ImportSpecList SrcSpanInfo] -> Maybe (A.ImportSpecList SrcSpanInfo)
            mergeSpecLists (A.ImportSpecList loc flag specs : ys) =
                Just (A.ImportSpecList loc flag (mergeSpecs (sortBy compareSpecs (nub (concat (specs : Prelude.map (\ (A.ImportSpecList _ _ specs') -> specs') ys))))))
            mergeSpecLists [] = error "mergeSpecLists"
      expandSDTypes :: A.ImportDecl SrcSpanInfo -> A.ImportDecl SrcSpanInfo
      expandSDTypes i@(A.ImportDecl {A.importSpecs = Just (A.ImportSpecList l f specs)}) =
          i {A.importSpecs = Just (A.ImportSpecList l f (Prelude.map (expandSpec i) specs))}
      expandSDTypes i = i
      expandSpec i s =
          if not (A.importQualified i) && member (Nothing, fmap (const ()) n) sdTypes ||
             maybe False (\ mn -> (member (Just (fmap (const ()) mn), fmap (const ()) n) sdTypes)) (A.importAs i) ||
             member (Just (fmap (const ()) (A.importModule i)), fmap (const ()) n) sdTypes
          then s'
          else s
          where
            n = case s of
                  (A.IVar _ x) -> x
                  (A.IAbs _ _ x) -> x
                  (A.IThingAll _ x) -> x
                  (A.IThingWith _ x _) -> x
            s' = case s of
                  (A.IVar l x) -> A.IThingAll l x
                  (A.IAbs l _ x) -> A.IThingAll l x
                  (A.IThingWith l x _) -> A.IThingAll l x
                  (A.IThingAll _ _) -> s

      -- Eliminate imports that became empty
      -- importPred :: ImportDecl -> Bool
      importPred (A.ImportDecl _ mn _ _ _ _ _ (Just (A.ImportSpecList _ _ []))) =
          not remove || maybe False (isEmptyImport . A.importSpecs) (find ((== (unModuleName mn)) . unModuleName . A.importModule) oldImports)
          where
            isEmptyImport (Just (A.ImportSpecList _ _ [])) = True
            isEmptyImport _ = False
      importPred _ = True

      sdTypes :: Set (Maybe (S.ModuleName ()), S.Name ())
      sdTypes = standaloneDerivingTypes m

standaloneDerivingTypes :: ModuleInfo -> Set (Maybe (S.ModuleName ()), S.Name ())
standaloneDerivingTypes (ModuleInfo (A.XmlPage _ _ _ _ _ _ _) _ _ _ _) = error "standaloneDerivingTypes A.XmlPage"
standaloneDerivingTypes (ModuleInfo (A.XmlHybrid _ _ _ _ _ _ _ _ _) _ _ _ _) = error "standaloneDerivingTypes A.XmlHybrid"
standaloneDerivingTypes (ModuleInfo (A.Module _ _ _ _ decls) _ _ _ _) =
    unions (Prelude.map derivDeclTypes decls)

-- | Collect the declared types of a standalone deriving declaration.
class DerivDeclTypes a where
    derivDeclTypes :: a -> Set (Maybe (S.ModuleName ()), S.Name ())

instance DerivDeclTypes (A.Decl l) where
    derivDeclTypes (A.DerivDecl _ _ x) = derivDeclTypes x
    derivDeclTypes _ = empty

instance DerivDeclTypes (A.InstRule l) where
    derivDeclTypes (A.IRule _ _ _ x)  = derivDeclTypes x
    derivDeclTypes (A.IParen _ x) = derivDeclTypes x

instance DerivDeclTypes (A.InstHead l) where
    derivDeclTypes (A.IHCon _ _) = empty
    derivDeclTypes (A.IHParen _ x) = derivDeclTypes x
    derivDeclTypes (A.IHInfix _ x _op) = derivDeclTypes x
    derivDeclTypes (A.IHApp _ x y) = union (derivDeclTypes x) (derivDeclTypes y)

instance DerivDeclTypes (A.Type l) where
    derivDeclTypes (A.TyForall _ _ _ x) = derivDeclTypes x -- qualified type
    derivDeclTypes (A.TyFun _ x y) = union (derivDeclTypes x) (derivDeclTypes y) -- function type
    derivDeclTypes (A.TyTuple _ _ xs) = unions (Prelude.map derivDeclTypes xs) -- tuple type, possibly boxed
    derivDeclTypes (A.TyList _ x) =  derivDeclTypes x -- list syntax, e.g. [a], as opposed to [] a
    derivDeclTypes (A.TyApp _ x y) = union (derivDeclTypes x) (derivDeclTypes y) -- application of a type constructor
    derivDeclTypes (A.TyVar _ _) = empty -- type variable
    derivDeclTypes (A.TyCon _ (A.Qual _ m n)) = singleton (Just (fmap (const ()) m), fmap (const ()) n) -- named type or type constructor
       -- Unqualified names refer to imports without "qualified" or "as" values.
    derivDeclTypes (A.TyCon _ (A.UnQual _ n)) = singleton (Nothing, fmap (const ()) n)
    derivDeclTypes (A.TyCon _ _) = empty
    derivDeclTypes (A.TyParen _ x) = derivDeclTypes x -- type surrounded by parentheses
    derivDeclTypes (A.TyInfix _ x _op y) = union (derivDeclTypes x) (derivDeclTypes y) -- infix type constructor
    derivDeclTypes (A.TyKind _ x _) = derivDeclTypes x -- type with explicit kind signature
    derivDeclTypes (A.TyParArray _ x) = derivDeclTypes x
    derivDeclTypes (A.TyPromoted _ _) = empty
    derivDeclTypes (A.TyEquals _ _ _) = empty -- a ~ b, not clear how this related to standalone deriving
    derivDeclTypes (A.TySplice _ _) = empty
    derivDeclTypes (A.TyBang _ _ _ x) = derivDeclTypes x
    derivDeclTypes (A.TyWildCard _ _) = empty

-- | Compare the two import declarations ignoring the things that are
-- actually being imported.  Equality here indicates that the two
-- imports could be merged.
importMergable :: A.ImportDecl SrcSpanInfo -> A.ImportDecl SrcSpanInfo -> Ordering
importMergable a b =
    case (compare `on` noSpecs) a' b' of
      EQ -> EQ
      specOrdering ->
          case (compare `on` S.importModule) a' b' of
            EQ -> specOrdering
            moduleNameOrdering -> moduleNameOrdering
    where
      a' = fmap (const ()) a
      b' = fmap (const ()) b
      -- Return a version of an ImportDecl with an empty spec list and no
      -- source locations.  This will distinguish "import Foo as F" from
      -- "import Foo", but will let us group imports that can be merged.
      -- Don't merge hiding imports with regular imports.
      SrcLoc path _ _ = srcLoc a
      noSpecs :: S.ImportDecl () -> S.ImportDecl ()
      noSpecs x = x { S.importAnn = (),
                      S.importSpecs = case S.importSpecs x of
                                        Just (A.ImportSpecList () True _) -> Just (A.ImportSpecList () True []) -- hiding
                                        Just (A.ImportSpecList () False _) -> Nothing
                                        Nothing -> Nothing }

-- | Be careful not to try to compare objects with embeded SrcSpanInfo.
unModuleName :: A.ModuleName SrcSpanInfo -> String
unModuleName (A.ModuleName _ x) = x

-- Compare function used to sort the symbols within an import.
compareSpecs :: A.ImportSpec SrcSpanInfo -> A.ImportSpec SrcSpanInfo -> Ordering
compareSpecs a b =
    case compare (Set.map (Prelude.map toLower . nameString) $ Set.fromList $ symbolsDeclaredBy a)
                 (Set.map (Prelude.map toLower . nameString) $ Set.fromList $ symbolsDeclaredBy b) of
      EQ -> compare (fmap (const ()) a) (fmap (const ()) b)
      x -> x

-- equalSpecs :: A.ImportSpec SrcSpanInfo -> A.ImportSpec SrcSpanInfo -> Bool
-- equalSpecs a b = compareSpecs a b == EQ

-- Merge elements of a sorted spec list as possible
-- unimplemented, should merge Foo and Foo(..) into Foo(..), and the like
mergeSpecs :: [A.ImportSpec SrcSpanInfo] -> [A.ImportSpec SrcSpanInfo]
mergeSpecs [] = []
mergeSpecs [x] = [x]
{-
-- We need to do this using the simplified syntax
mergeSpecs (x : y : zs) =
    case (name x' == name y', x, y) of
      (True, S.IThingAll _ _, _) -> mergeSpecs (x : zs)
      (True, _, S.IThingAll _ _) -> mergeSpecs (y : zs)
      (True, S.IThingWith _ n xs, S.IThingWith _ ys) -> mergeSpecs (S.IThingWith n (nub (xs ++ ys)))
      (True, S.IThingWith _ _, _) -> mergeSpecs (x' : zs)
      (True, _, S.IThingWith _ _) -> mergeSpecs (y' : zs)
      _ -> x : mergeSpecs (y : zs)
    where
      x' = sImportSpec x
      y' = sImportSpec y
      name (S.IVar n) = n
      name (S.IAbs n) = n
      name (S.IThingAll n) = n
      name (S.IThingWith n _) = n
-}
mergeSpecs xs = xs

-- dropSuffix :: Eq a => [a] -> [a] -> [a]
-- dropSuffix suf x = if isSuffixOf suf x then take (length x - length suf) x else x

-- dropPrefix :: Eq a => [a] -> [a] -> [a]
-- dropPrefix pre x = if isPrefixOf pre x then drop (length x) x else x

nameString :: S.Name () -> String
nameString (S.Ident () s) = s
nameString (S.Symbol () s) = s
