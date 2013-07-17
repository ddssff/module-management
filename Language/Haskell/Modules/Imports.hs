{-# LANGUAGE PackageImports, ScopedTypeVariables, StandaloneDeriving, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Imports
    ( cleanImports
    , cleanResults
    ) where

import Control.Applicative ((<$>))
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (bracket, catch, throw)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (find, groupBy, intercalate, nub, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>), mempty)
import Data.Sequence ((|>))
import Data.Set as Set (empty, fromList, member, Set, singleton, toList, union, unions)
import Language.Haskell.Exts.Annotated.Simplify as S (sImportDecl, sImportSpec, sModuleName, sName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(DerivDecl), ImportDecl(..), ImportSpec(..), ImportSpecList(ImportSpecList), InstHead(..), Module(..), ModuleHead(..), ModuleName(..), QName(..), Type(..))
import Language.Haskell.Exts.Extension (Extension(PackageImports))
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ImportDecl(importLoc, importModule, importSpecs), ModuleName(..), Name(..))
import Language.Haskell.Modules.Common (ModuleResult(..))
import Language.Haskell.Modules.Fold (foldDecls, foldExports, foldHeader, foldImports)
import Language.Haskell.Modules.ModuVerse (findModule, getExtensions, loadModule, modifyExtensions, ModuleInfo(..), moduleName, parseModule)
import Language.Haskell.Modules.Params (markForDelete, MonadClean(getParams), Params(hsFlags, removeEmptyImports, scratchDir, testMode))
import Language.Haskell.Modules.SourceDirs (modifyDirs, pathKey, PathKey(..), PathKey(unPathKey), SourceDirs(getDirs, putDirs))
import Language.Haskell.Modules.Util.DryIO (replaceFile, tildeBackup)
import Language.Haskell.Modules.Util.QIO (qLnPutStr, quietly)
import Language.Haskell.Modules.Util.SrcLoc (srcLoc)
import Language.Haskell.Modules.Util.Symbols (symbols)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode, showCommandForUser)

{-
-- | This is designed to be called from the postConf script of your
-- Setup file, it cleans up the imports of all the source files in the
-- package.

cleanBuildImports :: LocalBuildInfo -> IO ()
cleanBuildImports lbi =
    mapM (toFilePath srcDirs) (maybe [] exposedModules (library (localPkgDescr lbi))) >>= \ libPaths ->
    runCleanT (Distribution.Simple.LocalBuildInfo.scratchDir lbi) $ mapM_ clean (libPaths ++ exePaths)
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

-- | Clean up the imports of a source file.
cleanImports :: MonadClean m => [FilePath] -> m [ModuleResult]
cleanImports paths =
    do keys <- mapM pathKey paths >>= return . fromList
       dumpImports keys
       mapM (\ key -> parseModule key >>= checkImports) (toList keys)

cleanResults :: MonadClean m => [ModuleResult] -> m [ModuleResult]
cleanResults results =
    do mode <- getParams >>= return . testMode
       if mode then return results else (dump >> clean)
    where
      dump =
          mapM (\ x -> case x of
                         Removed _ _ -> return Nothing
                         Unchanged _ _ -> return Nothing
                         Modified _ key _ -> return (Just key)
                         Created name _ -> findModule name >>= return . fmap key_) results >>=
          dumpImports . fromList . catMaybes
      clean =
          mapM (\ x -> case x of
                         Removed _ _ -> return x
                         Unchanged _ _ -> return x
                         Modified _name key _ -> doModule key
                         Created name _ ->
                             do mi <- findModule name
                                let Just k = fmap key_ mi -- This is pretty sure to be a Just
                                x' <- doModule k
                                return $ toCreated x') results
      -- The cleaning may have turned a Created result into Modified,
      -- turn it back into Created.
      toCreated (Modified name _key text) = Created name text
      toCreated x@(Created {}) = x
      toCreated _ = error "toCreated"
      -- Update the cached version of the now modified module and then
      -- clean its import list.
      doModule key =
          do info <- loadModule key
             checkImports info

-- | Run ghc with -ddump-minimal-imports and capture the resulting .imports file.
dumpImports :: MonadClean m => Set PathKey -> m ()
dumpImports keys =
    do scratch <- scratchDir <$> getParams
       liftIO $ createDirectoryIfMissing True scratch
       let cmd = "ghc"
       args <- hsFlags <$> getParams
       dirs <- getDirs
       exts <- getExtensions
       let args' = args ++
                   ["--make", "-c", "-ddump-minimal-imports", "-outputdir", scratch, "-i" ++ intercalate ":" dirs] ++
                   map (("-X" ++) . show) exts ++
                   map unPathKey (toList keys)
       (code, _out, err) <- liftIO $ readProcessWithExitCode cmd args' ""
       case code of
         ExitSuccess -> quietly (qLnPutStr (showCommandForUser cmd args' ++ " -> Ok")) >> return ()
         ExitFailure _ -> error ("dumpImports: compile failed\n " ++ showCommandForUser cmd args' ++ " ->\n" ++ err)

-- | Parse the import list generated by GHC, parse the original source
-- file, and if all goes well insert the new imports into the old
-- source file.  We also need to modify the imports of any names
-- that are types that appear in standalone instance derivations so
-- their members are imported too.
checkImports :: MonadClean m => ModuleInfo -> m ModuleResult
checkImports info@(ModuleInfo (A.Module _ mh _ imports _) _ _ _) =
    do let importsPath = maybe "Main" (\ (A.ModuleHead _ (A.ModuleName _ s) _ _) -> s) mh ++ ".imports"
       -- The .imports file will appear in the real current directory,
       -- ignore the source dir path.  This may change in future
       -- versions of GHC, see http://ghc.haskell.org/trac/ghc/ticket/7957
       markForDelete importsPath
       (ModuleInfo newImports _ _ _) <-
           withDot $
             withPackageImportsExtension $
               (parseModule (PathKey importsPath)
                  `IO.catch` (\ (e :: IOError) -> liftIO (getCurrentDirectory >>= \ here ->
                                                          throw . userError $ here ++ ": " ++ show e)))
       updateSource info newImports extraImports
    where
      extraImports = filter isHiddenImport imports
      isHiddenImport (A.ImportDecl {A.importSpecs = Just (A.ImportSpecList _ True _)}) = True
      isHiddenImport _ = False
checkImports _ = error "Unsupported module type"

withPackageImportsExtension :: MonadClean m => m a -> m a
withPackageImportsExtension a =
    bracket (getExtensions)
            (modifyExtensions . const)
            (\ saved -> modifyExtensions (const (PackageImports : saved)) >> a)

withDot :: MonadClean m => m a -> m a
withDot a =
    bracket (getDirs)
            (modifyDirs . const)
            (\ _ -> putDirs ["."] >> a)

-- | If all the parsing went well and the new imports differ from the
-- old, update the source file with the new imports.
updateSource :: MonadClean m => ModuleInfo -> A.Module SrcSpanInfo -> [A.ImportDecl SrcSpanInfo] -> m ModuleResult
updateSource m@(ModuleInfo (A.Module _ _ _ oldImports _) _ _ key) (A.Module _ _ _ newImports _) extraImports =
    do remove <- removeEmptyImports <$> getParams
       maybe (qLnPutStr ("cleanImports: no changes to " ++ show key) >> return (Unchanged (moduleName m) key))
             (\ text' ->
                  qLnPutStr ("cleanImports: modifying " ++ show key) >>
                  replaceFile tildeBackup (unPathKey key) text' >>
                  return (Modified (moduleName m) key text'))
             (replaceImports (fixNewImports remove m oldImports (newImports ++ extraImports)) m)
updateSource _ _ _ = error "updateSource"

-- | Compare the old and new import sets and if they differ clip out
-- the imports from the sourceText and insert the new ones.
replaceImports :: [A.ImportDecl SrcSpanInfo] -> ModuleInfo -> Maybe String
replaceImports newImports m =
    let oldPretty = fold (foldImports (\ _ pref s suff r -> r |> (pref <> s <> suff)) m mempty)
        -- Surround newPretty with the same prefix and suffix as oldPretty
        newPretty = fromMaybe "" (foldImports (\ _ pref _ _ r -> maybe (Just pref) Just r) m Nothing) <>
                    intercalate "\n" (map (prettyPrintWithMode (defaultMode {layout = PPInLine})) newImports) <>
                    foldImports (\ _ _ _ suff _ -> suff) m mempty in
    if oldPretty == newPretty
    then Nothing
    else Just (fold (foldHeader (\ s r -> r |> s) (\ _ pref s suff r -> r |> (pref <> s <> suff))
                                (\ _ pref s suff r -> r |> pref <> s <> suff)
                                (\ _ pref s suff r -> r |> pref <> s <> suff) m mempty) ++
               fold (foldExports (\ s r -> r |> s)
                                 (\ _ pref s suff r -> r |> pref <> s <> suff)
                                 (\ s r -> r |> s) m mempty) ++
               newPretty <>
               fold (foldDecls  (\ _ pref s suff r -> r |> pref <> s <> suff) (\ r s -> s |> r) m mempty))

-- | Final touch-ups - sort and merge similar imports.
fixNewImports :: Bool         -- ^ If true, imports that turn into empty lists will be removed
              -> ModuleInfo
              -> [A.ImportDecl SrcSpanInfo]
              -> [A.ImportDecl SrcSpanInfo]
              -> [A.ImportDecl SrcSpanInfo]
fixNewImports remove m oldImports imports =
    filter importPred $ map expandSDTypes $ map mergeDecls $ groupBy (\ a b -> importMergable a b == EQ) $ sortBy importMergable imports
    where
      -- mergeDecls :: [ImportDecl] -> ImportDecl
      mergeDecls [] = error "mergeDecls"
      mergeDecls xs@(x : _) = x {A.importSpecs = mergeSpecLists (catMaybes (map A.importSpecs xs))}
          where
            -- Merge a list of specs for the same module
            mergeSpecLists :: [A.ImportSpecList SrcSpanInfo] -> Maybe (A.ImportSpecList SrcSpanInfo)
            mergeSpecLists (A.ImportSpecList loc flag specs : ys) =
                Just (A.ImportSpecList loc flag (mergeSpecs (sortBy compareSpecs (nub (concat (specs : map (\ (A.ImportSpecList _ _ specs') -> specs') ys))))))
            mergeSpecLists [] = error "mergeSpecLists"
      expandSDTypes :: A.ImportDecl SrcSpanInfo -> A.ImportDecl SrcSpanInfo
      expandSDTypes i@(A.ImportDecl {A.importSpecs = Just (A.ImportSpecList l f specs)}) =
          i {A.importSpecs = Just (A.ImportSpecList l f (map (expandSpec i) specs))}
      expandSDTypes i = i
      expandSpec i s =
          if not (A.importQualified i) && member (Nothing, sName n) sdTypes ||
             maybe False (\ mn -> (member (Just (sModuleName mn), sName n) sdTypes)) (A.importAs i) ||
             member (Just (sModuleName (A.importModule i)), sName n) sdTypes
          then s'
          else s
          where
            n = case s of
                  (A.IVar _ x) -> x
                  (A.IAbs _ x) -> x
                  (A.IThingAll _ x) -> x
                  (A.IThingWith _ x _) -> x
            s' = case s of
                  (A.IVar l x) -> A.IThingAll l x
                  (A.IAbs l x) -> A.IThingAll l x
                  (A.IThingWith l x _) -> A.IThingAll l x
                  (A.IThingAll _ _) -> s

      -- Eliminate imports that became empty
      -- importPred :: ImportDecl -> Bool
      importPred (A.ImportDecl _ mn _ _ _ _ (Just (A.ImportSpecList _ _ []))) =
          not remove || maybe False (isEmptyImport . A.importSpecs) (find ((== (unModuleName mn)) . unModuleName . A.importModule) oldImports)
          where
            isEmptyImport (Just (A.ImportSpecList _ _ [])) = True
            isEmptyImport _ = False
      importPred _ = True

      sdTypes :: Set (Maybe S.ModuleName, S.Name)
      sdTypes = standaloneDerivingTypes m

standaloneDerivingTypes :: ModuleInfo -> Set (Maybe S.ModuleName, S.Name)
standaloneDerivingTypes (ModuleInfo (A.XmlPage _ _ _ _ _ _ _) _ _ _) = error "standaloneDerivingTypes A.XmlPage"
standaloneDerivingTypes (ModuleInfo (A.XmlHybrid _ _ _ _ _ _ _ _ _) _ _ _) = error "standaloneDerivingTypes A.XmlHybrid"
standaloneDerivingTypes (ModuleInfo (A.Module _ _ _ _ decls) _ _ _) =
    unions (map derivDeclTypes decls)
    where
      -- derivDeclTypes :: Decl -> Set (Maybe S.ModuleName, S.Name)
      derivDeclTypes (A.DerivDecl _ _ (A.IHead _ _ xs)) = unions (map derivDeclTypes' xs) -- Just (moduleName, sName x)
      derivDeclTypes (A.DerivDecl a b (A.IHParen _ x)) = derivDeclTypes (A.DerivDecl a b x)
      derivDeclTypes (A.DerivDecl _ _ (A.IHInfix _ x _op y)) = union (derivDeclTypes' x) (derivDeclTypes' y)
      derivDeclTypes _ = empty
      -- derivDeclTypes' :: Type -> Set (Maybe S.ModuleName, S.Name)
      derivDeclTypes' (A.TyForall _ _ _ x) = derivDeclTypes' x -- qualified type
      derivDeclTypes' (A.TyFun _ x y) = union (derivDeclTypes' x) (derivDeclTypes' y) -- function type
      derivDeclTypes' (A.TyTuple _ _ xs) = unions (map derivDeclTypes' xs) -- tuple type, possibly boxed
      derivDeclTypes' (A.TyList _ x) =  derivDeclTypes' x -- list syntax, e.g. [a], as opposed to [] a
      derivDeclTypes' (A.TyApp _ x y) = union (derivDeclTypes' x) (derivDeclTypes' y) -- application of a type constructor
      derivDeclTypes' (A.TyVar _ _) = empty -- type variable
      derivDeclTypes' (A.TyCon _ (A.Qual _ m n)) = singleton (Just (sModuleName m), sName n) -- named type or type constructor
      -- Unqualified names refer to imports without "qualified" or "as" values.
      derivDeclTypes' (A.TyCon _ (A.UnQual _ n)) = singleton (Nothing, sName n)
      derivDeclTypes' (A.TyCon _ _) = empty
      derivDeclTypes' (A.TyParen _ x) = derivDeclTypes' x -- type surrounded by parentheses
      derivDeclTypes' (A.TyInfix _ x _op y) = union (derivDeclTypes' x) (derivDeclTypes' y) -- infix type constructor
      derivDeclTypes' (A.TyKind _ x _) = derivDeclTypes' x -- type with explicit kind signature

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
      a' = sImportDecl a
      b' = sImportDecl b
      -- Return a version of an ImportDecl with an empty spec list and no
      -- source locations.  This will distinguish "import Foo as F" from
      -- "import Foo", but will let us group imports that can be merged.
      -- Don't merge hiding imports with regular imports.
      SrcLoc path _ _ = srcLoc a
      noSpecs :: S.ImportDecl -> S.ImportDecl
      noSpecs x = x { S.importLoc = SrcLoc path 1 1, -- can we just use srcLoc a?
                      S.importSpecs = case S.importSpecs x of
                                        Just (True, _) -> Just (True, []) -- hiding
                                        Just (False, _) -> Nothing
                                        Nothing -> Nothing }

-- | Be careful not to try to compare objects with embeded SrcSpanInfo.
unModuleName :: A.ModuleName SrcSpanInfo -> String
unModuleName (A.ModuleName _ x) = x

-- Compare function used to sort the symbols within an import.
compareSpecs :: A.ImportSpec SrcSpanInfo -> A.ImportSpec SrcSpanInfo -> Ordering
compareSpecs a b =
    case compare (map (map toLower . nameString) $ catMaybes $ toList $ symbols a) (map (map toLower . nameString) $ catMaybes $ toList $ symbols b) of
      EQ -> compare (sImportSpec a) (sImportSpec b)
      x -> x

equalSpecs :: A.ImportSpec SrcSpanInfo -> A.ImportSpec SrcSpanInfo -> Bool
equalSpecs a b = compareSpecs a b == EQ

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

nameString :: S.Name -> String
nameString (S.Ident s) = s
nameString (S.Symbol s) = s
