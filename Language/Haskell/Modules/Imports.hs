{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
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
import Data.List (find, groupBy, intercalate, nub, sortBy)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Distribution.ModuleName as D (components, ModuleName)
import Distribution.PackageDescription (BuildInfo(hsSourceDirs), Executable, Library(exposedModules, libBuildInfo), PackageDescription(executables, library))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr, scratchDir)
import Language.Haskell.Exts.Annotated (defaultParseMode, parseFileWithComments, parseFileWithMode, ParseResult(..))
import Language.Haskell.Exts.Annotated.Simplify as S (sImportDecl, sModuleName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ImportDecl(ImportDecl, importModule, importSpecs), ImportSpecList(ImportSpecList), Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName), Name(..))
import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Extension (Extension(PackageImports))
import Language.Haskell.Exts.Parser (ParseMode(extensions))
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrintWithMode)
import Language.Haskell.Modules.Common (HasSymbols(symbols), ImportDecl, ImportSpec, ImportSpecList, Module, ModuleName, replaceFile, tildeBackup, withCurrentDirectory, ModuleResult(..))
import Language.Haskell.Modules.Fold (foldModule)
import Language.Haskell.Modules.Params (dryRun, hsFlags, markForDelete, MonadClean, removeEmptyImports, runCleanT, scratchDir, findSourcePath, sourceDirs, modulePath, modulePathBase, quietly, qPutStrLn)
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
    do source <- liftIO $ parseFileWithComments defaultParseMode path
       case source of
         ParseOk (m@(A.Module _ h _ _ _), comments) ->
             do let name = case h of
                             Just (A.ModuleHead _ x _ _) -> sModuleName x
                             _ -> S.ModuleName "Main"
                dumpImports path name >> checkImports path name m
         ParseOk ((A.XmlPage {}), _) -> error "cleanImports: XmlPage"
         ParseOk ((A.XmlHybrid {}), _) -> error "cleanImports: XmlHybrid"
         ParseFailed _loc msg -> error ("cleanImports: - parse of " ++ path ++ " failed: " ++ msg)

-- | Clean up the imports of a source file.
{-
cleanImports :: MonadClean m => S.ModuleName -> m ModuleResult
cleanImports name@(S.ModuleName x) =
    dumpImports name >> checkImports name
    do result <- try (dumpImports name >> checkImports name)
       qPutStrLn . either show (\ text -> x ++ ": " ++ maybe "no changes" (\ _ -> " updated") text) $ result
       return result
      -- `catch` (\ (e :: IOError) -> liftIO (hPutStrLn stderr (show e)))
-}

dumpImports :: MonadClean m => FilePath -> S.ModuleName -> m ()
dumpImports path name =
    do scratch <- Language.Haskell.Modules.Params.scratchDir
       liftIO $ createDirectoryIfMissing True scratch
       let cmd = "ghc"
       args <- hsFlags
       dirs <- sourceDirs
       let args' = args ++ ["--make", "-c", "-ddump-minimal-imports", "-outputdir", scratch, "-i" ++ intercalate ":" dirs, path]
       (code, _out, err) <- liftIO $ readProcessWithExitCode cmd args' ""
       case code of
         ExitSuccess -> quietly (qPutStrLn (showCommandForUser cmd args' ++ " -> Ok")) >> return ()
         ExitFailure _ -> error ("dumpImports: compile failed\n " ++ showCommandForUser cmd args' ++ " ->\n" ++ err)

-- | Parse the import list generated by GHC, parse the original source
-- file, and if all goes well insert the new imports into the old
-- source file.
checkImports :: MonadClean m => FilePath -> S.ModuleName -> Module -> m ModuleResult
checkImports path name@(S.ModuleName name') m@(A.Module _ h _ _ _) =
    do let importsPath = name' <.> ".imports"
       markForDelete importsPath
       result <- liftIO (parseFileWithMode (defaultParseMode {extensions = [PackageImports] ++ extensions defaultParseMode}) importsPath)
                   `catch` (\ (e :: IOError) -> liftIO getCurrentDirectory >>= \ here -> liftIO . throw . userError $ here ++ ": " ++ show e)
       case result of
         ParseOk newImports -> updateSource m newImports name
         _ -> error ("checkImports: parse of " ++ importsPath ++ " failed - " ++ show result)

-- | If all the parsing went well and the new imports differ from the
-- old, update the source file with the new imports.
updateSource :: MonadClean m => Module -> Module -> S.ModuleName -> m ModuleResult
updateSource (m@(A.Module _ _ _ oldImports _)) (A.Module _ _ _ newImports _) name =
    do remove <- removeEmptyImports
       dry <- dryRun
       sourcePath <- modulePath name
       sourceText <- liftIO $ readFile sourcePath
       maybe (qPutStrLn ("cleanImports: no changes to " ++ sourcePath) >> return (Unchanged name))
             (\ text ->
                  qPutStrLn ("cleanImports: modifying " ++ sourcePath) >>
                  liftIO (when (not dry) (replaceFile tildeBackup sourcePath text)) >>
                  return (Modified name text))
             (replaceImports (fixNewImports remove oldImports newImports) m sourceText)
updateSource _ _ _ = error "updateSource"

-- | Compare the old and new import sets and if they differ clip out
-- the imports from the sourceText and insert the new ones.
replaceImports :: [ImportDecl] -> Module -> String -> Maybe String
replaceImports newImports m sourceText =
    let newPretty = intercalate "\n" (map (prettyPrintWithMode (defaultMode {layout = PPInLine})) newImports)
        (before, before' : oldPretty, after) =
            foldModule (\ _ pre s (l, i, r) -> (l <> pre <> s, i, r))
                       (\ _ pre s (l, i, r) -> (l <> pre <> s, i, r))
                       (\ _ pre s (l, i, r) -> (l <> pre <> s, i, r))
                       (\ _ pre s (l, i, r) -> (l <> pre <> s, i, r))
                       (\ _ pre s (l, i, r) -> (l, i <> [pre, s], r))
                       (\ _ pre s (l, i, r) -> (l, i, r <> pre <> s))
                       (\ s (l, i, r) -> (l, i, r <> s))
                       m sourceText ("", [], "") in
    if newPretty /= concat oldPretty then Just (before <> before' <> newPretty <> after) else Nothing

-- | Final touch-ups - sort and merge similar imports.
fixNewImports :: Bool -> [ImportDecl] -> [ImportDecl] -> [ImportDecl]
fixNewImports remove oldImports imports =
    filter filterDecls $ map mergeDecls $ groupBy (\ a b -> importMergable a b == EQ) $ sortBy importMergable imports
    where
      mergeDecls :: [ImportDecl] -> ImportDecl
      mergeDecls xs@(x : _) = x {A.importSpecs = mergeSpecs (catMaybes (map A.importSpecs xs))}
      mergeDecls [] = error "mergeDecls"
      mergeSpecs :: [ImportSpecList] -> Maybe ImportSpecList
      mergeSpecs (A.ImportSpecList loc flag specs : xs) = Just (A.ImportSpecList loc flag (sortBy compareSpecs (nub (concat (specs : map (\ (A.ImportSpecList _ _ specs') -> specs') xs)))))
      mergeSpecs [] = error "mergeSpecs"
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
      noSpecs :: S.ImportDecl -> S.ImportDecl
      noSpecs x = x {S.importLoc = def, S.importSpecs = Nothing}

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
tests = TestLabel "Clean" (TestList [test1, test2])

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
