{-# LANGUAGE PackageImports, ScopedTypeVariables, StandaloneDeriving, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Imports
    ( cleanImports
    -- , cleanBuildImports
    , tests
    ) where

import Control.Monad (when)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, throw, try)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Default (def, Default)
import Data.Function (on)
import Data.List (find, groupBy, intercalate, nub, sortBy, (\\))
import Data.Map as Map (Map, empty, insertWith)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid ((<>))
import qualified Distribution.ModuleName as D (components, ModuleName)
import Distribution.PackageDescription (BuildInfo(hsSourceDirs), Executable, Library(exposedModules, libBuildInfo), PackageDescription(executables, library))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr, scratchDir)
import Language.Haskell.Exts.Annotated (defaultParseMode, parseFileWithMode, ParseResult(..))
import Language.Haskell.Exts.Annotated.Simplify as S (sImportDecl, sModuleName, sModule, sDecl, sName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ImportDecl(ImportDecl, importModule, importSpecs), ImportSpecList(ImportSpecList), ImportSpec(..), Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName), QName(..), Name(..), Decl(DerivDecl), InstHead(..), Type(..))
import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Extension (Extension(PackageImports, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances))
import Language.Haskell.Exts.Parser (ParseMode(extensions))
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrintWithMode)
import Language.Haskell.Modules.Common (HasSymbols(symbols), ImportDecl, ImportSpec, ImportSpecList, Module, ModuleName, Decl, QName, Type, replaceFile, tildeBackup, withCurrentDirectory, ModuleResult(..), Name)
import Language.Haskell.Modules.Fold (foldModule)
import Language.Haskell.Modules.Params (dryRun, hsFlags, markForDelete, MonadClean, removeEmptyImports, runCleanT, scratchDir, findSourcePath, sourceDirs, modulePath, modulePathBase, quietly, qPutStrLn, putSourceDirs, noisily, parseFileWithComments, extensions, modifyExtensions)
import System.Cmd (system)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((<.>), (</>))
import System.Process (readProcessWithExitCode, showCommandForUser)
import Test.HUnit (assertEqual, Test(..))

-- | This is designed to be called from the postConf script of your
-- Setup file, it cleans up the imports of all the source files in the
-- package.
{-
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
cleanImports :: MonadClean m => FilePath -> m ModuleResult
cleanImports path =
    do source <- parseFileWithComments path
       case source of
         ParseOk (m@(A.Module _ h _ imports decls), comments) ->
             do let name = case h of
                             Just (A.ModuleHead _ x _ _) -> sModuleName x
                             _ -> S.ModuleName "Main"
                    hiddenImports = filter isHiddenImport imports
                dumpImports path name >> checkImports path name m (hiddenImports ++ standaloneDerivingImports m)
         ParseOk ((A.XmlPage {}), _) -> error "cleanImports: XmlPage"
         ParseOk ((A.XmlHybrid {}), _) -> error "cleanImports: XmlHybrid"
         ParseFailed _loc msg -> error ("cleanImports: - parse of " ++ path ++ " failed: " ++ msg)
    where
      isHiddenImport (A.ImportDecl {A.importSpecs = Just (A.ImportSpecList _ True _)}) = True
      isHiddenImport _ = False

standaloneDerivingImports :: Module -> [ImportDecl]
standaloneDerivingImports m@(A.Module _ h _ imports decls) =
    mapMaybe filterTypes imports
    where
      filterTypes :: ImportDecl -> Maybe ImportDecl
      filterTypes imp =
          case A.importSpecs imp of
            Nothing -> Nothing
            Just (A.ImportSpecList sp False xs) ->
                case filter testSpec xs of
                  [] -> Nothing
                  ys -> Just (imp {A.importSpecs = Just (A.ImportSpecList sp False ys)})
      -- Is this spec one of the standalone deriving types?
      testSpec :: ImportSpec -> Bool
      testSpec (A.IVar _ x) = elem (moduleName, sName x) types
      testSpec (A.IAbs _ x) = elem (moduleName, sName x) types
      testSpec (A.IThingAll _ x) = elem (moduleName, sName x) types
      testSpec (A.IThingWith _ x _) = elem (moduleName, sName x) types
      -- The types that appear in this module in standalone deriving declarations
      types = concatMap derivDeclType decls
      derivDeclType :: Decl -> [(S.ModuleName, S.Name)]
      derivDeclType (A.DerivDecl _ _ (A.IHead _ _ xs)) = concatMap derivDeclType' xs -- Just (moduleName, sName x)
      derivDeclType (A.DerivDecl a b (A.IHParen _ x)) = derivDeclType (A.DerivDecl a b x)
      derivDeclType (A.DerivDecl _ _ (A.IHInfix _ x _op y)) = derivDeclType' x ++ derivDeclType' y
      derivDeclType _ = []
      derivDeclType' :: Type -> [(S.ModuleName, S.Name)]
      derivDeclType' (A.TyForall _ _ _ x) = derivDeclType' x -- qualified type
      derivDeclType' (A.TyFun _ x y) = derivDeclType' x ++ derivDeclType' y -- function type
      derivDeclType' (A.TyTuple _ _ xs) = concatMap derivDeclType' xs -- tuple type, possibly boxed
      derivDeclType' (A.TyList _ x) =  derivDeclType' x -- list syntax, e.g. [a], as opposed to [] a
      derivDeclType' (A.TyApp _ x y) = derivDeclType' x ++ derivDeclType' y -- application of a type constructor
      derivDeclType' (A.TyVar _ x) = [(moduleName, sName x)] -- type variable
      derivDeclType' (A.TyCon _ (A.Qual _ m n)) = [(sModuleName m, sName n)] -- named type or type constructor
      derivDeclType' (A.TyCon _ (A.UnQual _ n)) = [(moduleName, sName n)]
      derivDeclType' (A.TyCon _ _) = []
      derivDeclType' (A.TyParen _ x) = derivDeclType' x -- type surrounded by parentheses
      derivDeclType' (A.TyInfix _ x _op y) = derivDeclType' x ++ derivDeclType' y -- infix type constructor
      derivDeclType' (A.TyKind _ x _) = derivDeclType' x -- type with explicit kind signature
      moduleName = case h of
                     Just (A.ModuleHead _ x _ _) -> sModuleName x
                     _ -> S.ModuleName "Main"

findStandaloneDerivingSpecs :: [(S.ModuleName, S.Name)] -> ImportDecl -> Map S.ModuleName [ImportSpec] -> Map S.ModuleName [ImportSpec]
findStandaloneDerivingSpecs types imp@(A.ImportDecl _ moduleName _ _ _ _ Nothing) mp = mp
findStandaloneDerivingSpecs types imp@(A.ImportDecl _ moduleName _ _ _ _ (Just (A.ImportSpecList _ False specs))) mp =
    foldr f mp specs
    where
      f :: ImportSpec -> Map S.ModuleName [ImportSpec] -> Map S.ModuleName [ImportSpec]
      f spec mp | elem (sModuleName moduleName, sName (specName spec)) types = Map.insertWith (++) (sModuleName moduleName) [spec] mp
      f _ mp = mp

specName :: ImportSpec -> Name
specName (A.IVar _ name) = name
specName (A.IAbs _ name) = name
specName (A.IThingAll _ name) = name
specName (A.IThingWith _ name _) = name

findStandaloneDerivingTypes :: S.ModuleName -> Decl -> Maybe (S.ModuleName, S.Name)
findStandaloneDerivingTypes mname (A.DerivDecl _ _ h) =
    f h
    where
      f (A.IHead _ (A.UnQual _ x) _) = Just (mname, sName x)
      f (A.IHInfix _ _ (A.Qual _ mname' x) _) = Just (sModuleName mname', sName x)
      f (A.IHInfix _ _ (A.Special _ _) _) = Nothing
      f (A.IHParen _ x) = f x
findStandaloneDerivingTypes _ _ = Nothing

dumpImports :: MonadClean m => FilePath -> S.ModuleName -> m ()
dumpImports path name =
    do scratch <- Language.Haskell.Modules.Params.scratchDir
       liftIO $ createDirectoryIfMissing True scratch
       let cmd = "ghc"
       args <- hsFlags
       dirs <- sourceDirs
       exts <- Language.Haskell.Modules.Params.extensions
       let args' = args ++ ["--make", "-c", "-ddump-minimal-imports", "-outputdir", scratch, "-i" ++ intercalate ":" dirs, path] ++ map (("-X" ++) . show) exts
       (code, _out, err) <- liftIO $ readProcessWithExitCode cmd args' ""
       case code of
         ExitSuccess -> quietly (qPutStrLn (showCommandForUser cmd args' ++ " -> Ok")) >> return ()
         ExitFailure _ -> error ("dumpImports: compile failed\n " ++ showCommandForUser cmd args' ++ " ->\n" ++ err)

-- | Parse the import list generated by GHC, parse the original source
-- file, and if all goes well insert the new imports into the old
-- source file.
checkImports :: MonadClean m => FilePath -> S.ModuleName -> Module -> [ImportDecl] -> m ModuleResult
checkImports path name@(S.ModuleName name') m extraImports =
    do let importsPath = name' <.> ".imports"
       markForDelete importsPath
       result <- liftIO (parseFileWithMode (defaultParseMode {Language.Haskell.Exts.Parser.extensions = [PackageImports] ++ Language.Haskell.Exts.Parser.extensions defaultParseMode}) importsPath)
                   `catch` (\ (e :: IOError) -> liftIO getCurrentDirectory >>= \ here -> liftIO . throw . userError $ here ++ ": " ++ show e)
       case result of
         ParseOk newImports -> updateSource path m newImports name extraImports
         _ -> error ("checkImports: parse of " ++ importsPath ++ " failed - " ++ show result)

-- | If all the parsing went well and the new imports differ from the
-- old, update the source file with the new imports.
updateSource :: MonadClean m => FilePath -> Module -> Module -> S.ModuleName -> [ImportDecl] -> m ModuleResult
updateSource path (m@(A.Module _ _ _ oldImports _)) (A.Module _ _ _ newImports _) name extraImports =
    do remove <- removeEmptyImports
       dry <- dryRun
       -- sourcePath <- modulePath name
       text <- liftIO $ readFile path
       maybe (qPutStrLn ("cleanImports: no changes to " ++ path) >> return (Unchanged name))
             (\ text' ->
                  qPutStrLn ("cleanImports: modifying " ++ path) >>
                  liftIO (when (not dry) (replaceFile tildeBackup path text')) >>
                  return (Modified name text'))
             (replaceImports (fixNewImports remove oldImports (newImports ++ extraImports)) m text)
updateSource _ _ _ _ _ = error "updateSource"

-- | Compare the old and new import sets and if they differ clip out
-- the imports from the sourceText and insert the new ones.
replaceImports :: [ImportDecl] -> Module -> String -> Maybe String
replaceImports newImports m sourceText =
    let newPretty = intercalate "\n" (map (prettyPrintWithMode (defaultMode {layout = PPInLine})) newImports)
        (before, oldPretty', after) =
            foldModule (\ _ pre s (l, i, r) -> (l <> pre <> s, i, r))
                       (\ _ pre s (l, i, r) -> (l <> pre <> s, i, r))
                       (\ _ pre s (l, i, r) -> (l <> pre <> s, i, r))
                       (\ _ pre s (l, i, r) -> (l <> pre <> s, i, r))
                       (\ _ pre s (l, i, r) -> (l, i <> [pre, s], r))
                       (\ _ pre s (l, i, r) -> (l, i, r <> pre <> s))
                       (\ s (l, i, r) -> (l, i, r <> s))
                       m sourceText ("", [], "") in
    case oldPretty' of
      (before' : oldPretty) -> if newPretty /= concat oldPretty then Just (before <> before' <> newPretty <> after) else Nothing
      [] -> Nothing

-- | Final touch-ups - sort and merge similar imports.
fixNewImports :: Bool -> [ImportDecl] -> [ImportDecl] -> [ImportDecl]
fixNewImports remove oldImports imports =
    filter filterDecls $ map mergeDecls $ groupBy (\ a b -> importMergable a b == EQ) $ sortBy importMergable imports
    where
      mergeDecls :: [ImportDecl] -> ImportDecl
      mergeDecls xs@(x : _) = x {A.importSpecs = mergeSpecLists (catMaybes (map A.importSpecs xs))}
      mergeDecls [] = error "mergeDecls"
      -- Merge a list of specs for the same module
      mergeSpecLists :: [ImportSpecList] -> Maybe ImportSpecList
      mergeSpecLists (A.ImportSpecList loc flag specs : xs) = Just (A.ImportSpecList loc flag (mergeSpecs (sortBy compareSpecs (nub (concat (specs : map (\ (A.ImportSpecList _ _ specs') -> specs') xs))))))
      mergeSpecLists [] = error "mergeSpecLists"
      mergeSpecs xs = xs -- unimplemented, should merge Foo and Foo(..) into Foo(..), and the like
      filterDecls :: ImportDecl -> Bool
      filterDecls (A.ImportDecl _ m _ _ _ _ (Just (A.ImportSpecList _ _ []))) = not remove || maybe False (isEmptyImport . A.importSpecs) (find ((== (unModuleName m)) . unModuleName . A.importModule) oldImports)
      filterDecls _ = True
      isEmptyImport (Just (A.ImportSpecList _ _ [])) = True
      isEmptyImport _ = False

-- | Compare the two import declarations ignoring the things that are
-- actually being imported.  Equality here indicates that the two
-- imports could be merged.
importMergable :: ImportDecl -> ImportDecl -> Ordering
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
      noSpecs :: S.ImportDecl -> S.ImportDecl
      noSpecs x = x { S.importLoc = def,
                      S.importSpecs = case S.importSpecs x of
                                        Just (True, _) -> Just (True, []) -- hiding
                                        Just (False, _) -> Nothing
                                        Nothing -> Nothing }

-- | Be careful not to try to compare objects with embeded SrcSpanInfo.
unModuleName :: ModuleName -> String
unModuleName (A.ModuleName _ x) = x

-- Compare function used to sort the symbols within an import.
compareSpecs :: ImportSpec -> ImportSpec -> Ordering
compareSpecs a b =
    case compare (map (map toLower . nameString) $ symbols a) (map (map toLower . nameString) $ symbols b) of
      EQ -> compare a b
      x -> x

-- dropSuffix :: Eq a => [a] -> [a] -> [a]
-- dropSuffix suf x = if isSuffixOf suf x then take (length x - length suf) x else x

-- dropPrefix :: Eq a => [a] -> [a] -> [a]
-- dropPrefix pre x = if isPrefixOf pre x then drop (length x) x else x

nameString :: A.Name () -> String
nameString (A.Ident _ s) = s
nameString (A.Symbol _ s) = s

tests :: Test
tests = TestLabel "Clean" (TestList [test1, test2, test3, test4])

test1 :: Test
test1 =
    TestCase
      (do _ <- system "rsync -aHxS --delete testdata/original/ testdata/copy"
          let name = S.ModuleName "Debian.Repo.Types.PackageIndex"
          let base = modulePathBase name
          _ <- withCurrentDirectory "testdata/copy" (runCleanT "dist/scratch" (cleanImports base))
          (code, diff, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/original" </> base, "testdata/copy" </> base] ""
          assertEqual "cleanImports"
                         (ExitFailure 1,
                          ["@@ -22,13 +22,13 @@",
                           "     , prettyPkgVersion",
                           "     ) where",
                           " ",
                           "-import Data.Text (Text, map)",
                           "+import Data.Text (Text)",
                           " import Debian.Arch (Arch(..))",
                           " import qualified Debian.Control.Text as T (Paragraph)",
                           " import Debian.Relation (BinPkgName(..), SrcPkgName(..))",
                           " import qualified Debian.Relation as B (PkgName, Relations)",
                           " import Debian.Release (Section(..))",
                           "-import Debian.Repo.Orphans ({- instances -})",
                           "+import Debian.Repo.Orphans ()",
                           " import Debian.Version (DebianVersion, prettyDebianVersion)",
                           " import System.Posix.Types (FileOffset)",
                           " import Text.PrettyPrint.ANSI.Leijen ((<>), Doc, Pretty(pretty), text)"],
                          "")
                          (code, drop 2 (lines diff), err))

test2 :: Test
test2 =
    TestCase
      (do _ <- system "rsync -aHxS --delete testdata/ testcopy"
          let name = S.ModuleName "Debian.Repo.PackageIndex"
              base = modulePathBase name
          _ <- withCurrentDirectory "testdata/copy" (runCleanT "dist/scratch" (cleanImports base))
          (code, diff, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/original" </> base, "testdata/copy" </> base] ""
          assertEqual "cleanImports" (ExitSuccess, "", "") (code, diff, err))

-- | Can we handle a Main module in a file named something other than Main.hs?
test3 :: Test
test3 =
    TestCase
      (runCleanT "scratch" (noisily $ noisily $ putSourceDirs ["testdata"] >> cleanImports "testdata/NotMain.hs") >>
       assertEqual "module name" () ())

-- | Preserve imports with a "hiding" clause
test4 :: Test
test4 =
    TestCase
      (system "cp testdata/HidingOrig.hs testdata/Hiding.hs" >>
       runCleanT "scratch" (noisily $ noisily $ putSourceDirs ["testdata"] >> cleanImports "testdata/Hiding.hs") >>
       -- Need to check the text of Hiding.hs, but at least this verifies that there was no crash
       assertEqual "module name" () ())

-- | Preserve imports used by a standalone deriving declaration
test5 :: Test
test5 =
    TestCase
      (system "cp testdata/DerivingOrig.hs testdata/Deriving.hs" >>
       runCleanT "scratch" (noisily $ noisily $
                            modifyExtensions (++ [StandaloneDeriving, TypeSynonymInstances, FlexibleInstances]) >>
                            putSourceDirs ["testdata"] >> cleanImports "testdata/Deriving.hs") >>
       assertEqual "standalone deriving" () ())
