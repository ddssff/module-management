{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Clean
    ( cleanImports
    , cleanBuildImports
    , tests
    ) where

import Control.Monad (when)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (try, catch, throw)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Default (def, Default)
import Data.Function (on)
import Data.List (groupBy, intercalate, nub, sortBy, find)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Distribution.ModuleName as D (components, ModuleName)
import Distribution.PackageDescription (BuildInfo(hsSourceDirs), Executable, Executable(modulePath), Library(exposedModules, libBuildInfo), PackageDescription(executables, library))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr, scratchDir)
import Language.Haskell.Exts.Annotated (defaultParseMode, parseFileWithComments, parseFileWithMode, ParseResult(..))
import qualified Language.Haskell.Exts.Annotated.Syntax as A -- (ImportDecl(..), ImportSpec(..), Module(..), ModuleName(..), ModuleHead(..), Name(..))
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Extension (Extension(PackageImports))
import Language.Haskell.Exts.Parser (ParseMode(extensions))
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrintWithMode)
import Language.Haskell.Imports.Common (replaceFile, tildeBackup, withCurrentDirectory, HasSymbols(symbols),
                                        Module, ImportDecl, ImportSpecList, ImportSpec, ModuleName)
import Language.Haskell.Imports.Fold (foldModule)
import Language.Haskell.Imports.Params (dryRun, hsFlags, markForDelete, MonadParams, {-putDryRun,-} removeEmptyImports, runParamsT, scratchDir)
import System.Cmd (system)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((<.>), (</>))
import System.Process (readProcessWithExitCode, showCommandForUser)
import Test.HUnit (assertEqual, Test(..))

tests :: Test
tests = TestLabel "Clean" (TestList [test1, test2])

test1 :: Test
test1 =
    TestCase
      (do _ <- system "rsync -aHxS --delete testdata/original/ testdata/copy"
          let path = "Debian/Repo/Types/PackageIndex.hs"
          _ <- withCurrentDirectory "testdata/copy" (runParamsT "dist/scratch" (cleanImports path))
          (ExitFailure 1, diff, _) <- readProcessWithExitCode "diff" ["-ru", "testdata/original" </> path, "testdata/copy" </> path] ""
          assertEqual "cleanImports"
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
                           " import Text.PrettyPrint.ANSI.Leijen ((<>), Doc, Pretty(pretty), text)"]
                          (drop 2 (lines diff)))

test2 :: Test
test2 =
    TestCase
      (do _ <- system "rsync -aHxS --delete testdata/ testcopy"
          let path = "Debian/Repo/PackageIndex.hs"
          _ <- withCurrentDirectory "testcopy" (runParamsT "dist/scratch" (cleanImports path))
          (ExitSuccess, diff, _) <- readProcessWithExitCode "diff" ["-ru", "testdata" </> path, "testcopy" </> path] ""
          assertEqual "cleanImports" "" diff)

-- | This is designed to be called from the postConf script of your
-- Setup file, it cleans up the imports of all the source files in the
-- package.
cleanBuildImports :: LocalBuildInfo -> IO ()
cleanBuildImports lbi =
    mapM (toFilePath srcDirs) (maybe [] exposedModules (library (localPkgDescr lbi))) >>= \ libPaths ->
    runParamsT (Distribution.Simple.LocalBuildInfo.scratchDir lbi) $ mapM_ clean (libPaths ++ exePaths)
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

-- | Clean up the imports of a source file.
cleanImports :: MonadParams m => FilePath -> m (Either IOError (Maybe String))
cleanImports sourcePath =
    try (dumpImports sourcePath >> checkImports sourcePath)
      -- `catch` (\ (e :: IOError) -> liftIO (hPutStrLn stderr (show e)))


dumpImports :: MonadParams m => FilePath -> m ()
dumpImports sourcePath =
    do scratch <- Language.Haskell.Imports.Params.scratchDir
       liftIO $ createDirectoryIfMissing True scratch
       let cmd = "ghc"
       args' <- hsFlags >>= return . (["--make", "-c", "-ddump-minimal-imports", "-outputdir", scratch, sourcePath] ++)
       (code, _out, err) <- liftIO $ readProcessWithExitCode cmd args' ""
       case code of
         ExitSuccess -> return () -- liftIO $ putStrLn $ showCommandForUser cmd args' ++ " -> Ok"
         ExitFailure _ -> error (sourcePath ++ ": compile failed\n " ++ showCommandForUser cmd args' ++ " ->\n" ++ err)

-- | Parse the import list generated by GHC, parse the original source
-- file, and if all goes well insert the new imports into the old
-- source file.
checkImports :: MonadParams m => FilePath -> m (Maybe String)
checkImports sourcePath =
    do source <- liftIO $ parseFileWithComments defaultParseMode sourcePath
       sourceText <- liftIO $ readFile sourcePath
       case source of
         ParseOk (m@(A.Module _ (Just (A.ModuleHead _ (A.ModuleName _ name) _ _)) _ _ _), comments) ->
             do let importsPath = name <.> ".imports"
                markForDelete importsPath
                result <- liftIO (parseFileWithMode (defaultParseMode {extensions = [PackageImports] ++ extensions defaultParseMode}) importsPath) `catch` (\ (e :: IOError) -> liftIO getCurrentDirectory >>= \ here -> liftIO . throw . userError $ here ++ ": " ++ show e)
                case result of
                  ParseOk newImports -> updateSource newImports sourcePath m comments sourceText
                  _ -> error (importsPath ++ ": parse failed")
         ParseFailed _loc msg -> error (sourcePath ++ " - parse failed: " ++ msg)

-- | If all the parsing went well and the new imports differ from the
-- old, update the source file with the new imports.
updateSource :: MonadParams m => Module -> FilePath -> Module -> [Comment] -> String -> m (Maybe String)
updateSource _ sourcePath (A.Module _ Nothing _ _ _) _ _ =
    error (sourcePath ++ ": Won't modify source file with no explicit export list")
updateSource _ sourcePath (A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _) _ _ =
    error (sourcePath ++ ": Won't modify source file with no explicit export list")
updateSource (A.Module _ _ _ newImports _) sourcePath (m@(A.Module _ _ _ oldImports _)) _ sourceText =
    removeEmptyImports >>= \ remove ->
    dryRun >>= \ dry ->
    maybe (liftIO (putStrLn (sourcePath ++ ": no changes")) >> return Nothing)
          (\ text ->
               liftIO (putStrLn (sourcePath ++ ": replacing imports")) >>
               liftIO (when (not dry) (replaceFile tildeBackup sourcePath text)) >>
               return (Just text))
          (replaceImports (fixNewImports remove oldImports newImports) m sourceText {-(importsSpan m comments sourceText)-})

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
    filter filterDecls $ map mergeDecls $ groupBy ((==) `on` noSpecs) $ sortBy importCompare imports
    where
      importCompare a b =
          case (compare `on` A.importModule) a b of
            EQ -> (compare `on` noSpecs) a b
            x -> x
      noSpecs :: ImportDecl -> ImportDecl
      noSpecs x = x {A.importAnn = def, A.importSpecs = fmap (\ (A.ImportSpecList loc flag _) -> (A.ImportSpecList loc flag [])) (A.importSpecs x)}
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
