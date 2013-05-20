{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Clean
    ( cleanImports
    , cleanBuildImports
    , test1
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, MonadCatchIO, try)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Default (def, Default)
import Data.Function (on)
import Data.List (groupBy, intercalate, nub, sortBy)
import Data.Maybe (catMaybes)
import Data.Set (toList)
import qualified Distribution.ModuleName as D (components, ModuleName)
import Distribution.PackageDescription (BuildInfo(hsSourceDirs), Executable, Executable(modulePath), Library(exposedModules, libBuildInfo), PackageDescription(executables, library))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr, scratchDir)
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, parseFileWithMode, ParseResult(..))
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Extension (Extension(PackageImports))
import Language.Haskell.Exts.Parser (ParseMode(extensions))
import Language.Haskell.Exts.Syntax (ImportDecl(..), ImportSpec, Module(..), ModuleName(ModuleName))
import Language.Haskell.Imports.Common (replaceFile, tildeBackup)
import Language.Haskell.Imports.Params (dryRun, hsFlags, markForDelete, MonadParams, putDryRun, putScratchDir, removeEmptyImports, runParamsT, scratchDir, toDelete)
import Language.Haskell.Imports.Syntax (HasSymbol(symbol), importsSpan, nameString, replaceImports)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode, showCommandForUser)
import Test.HUnit (assertEqual, Test(TestCase))

test1 :: Test
test1 =
    TestCase
      (setCurrentDirectory "testdata" >>
       runParamsT (putDryRun True >> cleanImports "Debian/Repo/Package.hs") >>= \ result ->
       assertEqual
         "cleanImports"
         ()
         result)

-- | This is designed to be called from the postConf script of your
-- Setup file, it cleans up the imports of all the source files in the
-- package.
cleanBuildImports :: MonadParams m => LocalBuildInfo -> m ()
cleanBuildImports lbi =
    putScratchDir (Distribution.Simple.LocalBuildInfo.scratchDir lbi) >>
    mapM (liftIO . toFilePath srcDirs) (maybe [] exposedModules (library (localPkgDescr lbi))) >>= \ libPaths ->
    mapM_ cleanImports (libPaths ++ exePaths) >>
    toDelete >>= liftIO . mapM_ removeFile' . toList
    where
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
cleanImports :: MonadParams m => FilePath -> m ()
cleanImports sourcePath =
    (dumpImports >> checkImports sourcePath) `catch` (\ (e :: SomeException) -> liftIO (hPutStrLn stderr (show e)))
    where
      dumpImports =
          do scratch <- Language.Haskell.Imports.Params.scratchDir
             liftIO $ createDirectoryIfMissing True scratch
             let cmd = "ghc"
             args' <- hsFlags >>= return . (["--make", "-c", "-ddump-minimal-imports", "-outputdir", scratch, sourcePath] ++)
             (code, _out, err) <- liftIO $ readProcessWithExitCode cmd args' ""
             case code of
               ExitSuccess -> return ()
               ExitFailure _ -> error (sourcePath ++ ": compile failed\n " ++ showCommandForUser cmd args' ++ " ->\n" ++ err)

-- | Parse the import list generated by GHC, parse the original source
-- file, and if all goes well insert the new imports into the old
-- source file.
checkImports :: MonadParams m => FilePath -> m ()
checkImports sourcePath =
    do source <- liftIO $ try ((,) <$> parseFileWithComments defaultParseMode sourcePath <*> readFile sourcePath)
       case source of
         Right ((ParseOk (m@(Module _ (ModuleName name) _ _ _ _ _), comments)), sourceText) ->
             do let importsPath = name <.> ".imports"
                result <- liftIO (parseFileWithMode (defaultParseMode {extensions = [PackageImports] ++ extensions defaultParseMode}) importsPath)
                markForDelete importsPath
                case result of
                  ParseOk newImports -> updateSource newImports sourcePath m comments sourceText
                  _ -> error (importsPath ++ ": parse failed")
         Right _ -> error (sourcePath ++ ": parse failed")
         Left (e :: SomeException) -> error (sourcePath ++ ": " ++ show e)

removeFile' :: FilePath -> IO ()
removeFile' path = hPutStrLn stderr ("removeFile " ++ show path) >> removeFile path

-- | If all the parsing went well and the new imports differ from the
-- old, update the source file with the new imports.
updateSource :: MonadParams m => Module -> FilePath -> Module -> [Comment] -> String -> m ()
updateSource _ sourcePath (Module _ _ _ _ Nothing _ _) _ _ =
    error (sourcePath ++ ": Won't modify source file with no explicit export list")
updateSource _ sourcePath (Module _ _ _ _ _ _ []) _ _ =
    error (sourcePath ++ ": Won't modify source file with no declarations")
updateSource (Module _ _ _ _ _ newImports _) sourcePath (m@(Module _ _ _ _ _ oldImports _)) comments sourceText =
    removeEmptyImports >>= \ remove ->
    dryRun >>= \ dry ->
    maybe (liftIO (putStrLn (sourcePath ++ ": no changes")))
          (\ text ->
               liftIO (putStrLn (sourcePath ++ ": replacing imports")) >>
               liftIO (replaceFile dry tildeBackup sourcePath text))
          (replaceImports oldImports (fixNewImports remove newImports) sourceText (importsSpan m comments))

-- | Final touch-ups - sort and merge similar imports.
fixNewImports :: Bool -> [ImportDecl] -> [ImportDecl]
fixNewImports remove imports =
    filter filterDecls $ map mergeDecls $ groupBy ((==) `on` noSpecs) $ sortBy importCompare imports
    where
      importCompare a b =
          case (compare `on` importModule) a b of
            EQ -> (compare `on` noSpecs) a b
            x -> x
      noSpecs :: ImportDecl -> ImportDecl
      noSpecs x = x {importLoc = def, importSpecs = fmap (\ (flag, _) -> (flag, [])) (importSpecs x)}
      mergeDecls :: [ImportDecl] -> ImportDecl
      mergeDecls xs@(x : _) = x {importSpecs = mergeSpecs (catMaybes (map importSpecs xs))}
      mergeDecls [] = error "mergeDecls"
      mergeSpecs :: [(Bool, [ImportSpec])] -> Maybe (Bool, [ImportSpec])
      mergeSpecs (x : xs) = Just (fst x, sortBy compareSpecs (nub (concat (snd x : map snd xs))))
      mergeSpecs [] = error "mergeSpecs"
      filterDecls :: ImportDecl -> Bool
      filterDecls (ImportDecl _ _ _ _ _ _ (Just (_, []))) = not remove
      filterDecls _ = True

compareSpecs :: ImportSpec -> ImportSpec -> Ordering
compareSpecs a b =
    case compare (fmap (map toLower . nameString) $ symbol a) (fmap (map toLower . nameString) $ symbol b) of
      EQ -> compare a b
      x -> x

-- dropSuffix :: Eq a => [a] -> [a] -> [a]
-- dropSuffix suf x = if isSuffixOf suf x then take (length x - length suf) x else x

-- dropPrefix :: Eq a => [a] -> [a] -> [a]
-- dropPrefix pre x = if isPrefixOf pre x then drop (length x) x else x
