{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Clean
    ( cleanImports
    , cleanBuildImports
    , test1
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (try, catch, MonadCatchIO)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (groupBy, intercalate, isSuffixOf, nub, sortBy)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Set (toList)
import qualified Distribution.ModuleName as D (components, ModuleName)
import Distribution.PackageDescription (BuildInfo(hsSourceDirs), Executable, Executable(modulePath), Library(exposedModules, libBuildInfo), PackageDescription(executables, library))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr, scratchDir)
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Extension (Extension(PackageImports))
import Language.Haskell.Exts.Syntax (ImportDecl(importSpecs, importModule), ImportSpec, Module(..), ModuleName(ModuleName))
import Language.Haskell.Exts.Parser (ParseMode(extensions))
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, parseFileWithMode, ParseResult(..))
import Language.Haskell.Imports.Common (importsSpan, replaceFile, replaceImports, specName)
import Language.Haskell.Imports.Params (MonadParams, runParamsT, putDryRun, hsFlags, putJunk, putScratchJunk, junk, putScratchDir, scratchDir)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode, showCommandForUser)

test1 :: IO ()
test1 = runParamsT (putDryRun True >> cleanImports "Language/Haskell/Imports/Clean.hs")

-- | To use this function you must first make sure...
--    1. There are up-to-date .imports files in the top directory, generated when
--       the -ddump-minimal-imports flag is added to ghc-options.
--    2. The first import must be preceded by a blank line
--    3. The last import must be followed by a blank line
--    4. No imports commented out with {- -} appear anywhere
-- It will not operate on files without an explicit export list or on
-- files with no declarations.  To use it do the following:
--    1. Set the postConf value in Setup  to:
--         @\ _ _ _ lbi -> cleanDumpedImports lbi@
--    1. Add -ddump-minimal-imports to the GHC-Options value in your
--       cabal file.  Then do a Setup build to generate the .imports
--       files.
--    2. Next do a Setup configure to update the source files
--    3. Do another Setup build to make sure everything still builds.
--       If there are problems with a module, either edit the source
--       file or move the backup file (suffix ~) back to where the
--       original was.
-- Bug: it removes declarations like "import Prelude hiding (last)"
cleanBuildImports :: MonadParams m => LocalBuildInfo -> m ()
cleanBuildImports lbi =
    putScratchDir (Distribution.Simple.LocalBuildInfo.scratchDir lbi) >>
    mapM (liftIO . toFilePath srcDirs) (maybe [] exposedModules (library (localPkgDescr lbi))) >>= \ libPaths ->
    mapM_ cleanImports (libPaths ++ exePaths) >>
    junk >>= liftIO . mapM_ removeFile' . toList
    where
      exePaths = map modulePath (executables (localPkgDescr lbi))
      -- libModules :: [D.ModuleName]
      -- libModules = maybe [] exposedModules (library (localPkgDescr lbi))
      -- exeModules :: [D.ModuleName]
      -- exeModules = 
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
    (do scratch <- Language.Haskell.Imports.Params.scratchDir
        liftIO $ createDirectoryIfMissing True scratch
        dump scratch
        replace) `catch` (\ (e :: SomeException) -> liftIO (hPutStrLn stderr (show e)))
    where
      dump scratch =
          do args' <- hsFlags >>= return . (args ++)
             (code, _out, err) <- liftIO $ readProcessWithExitCode cmd args' ""
             case code of
               ExitSuccess -> return ()
               ExitFailure _ -> error (sourcePath ++ ": compile failed\n " ++ showCommandForUser cmd args' ++ " ->\n" ++ err)
          where
            cmd = "ghc"
            args = ["--make", "-ddump-minimal-imports", "-outputdir", scratch, sourcePath]

      replace = checkImports sourcePath

-- | Parse the import list generated by GHC, parse the original source
-- file, and if all goes well insert the new imports into the old
-- source file.
checkImports :: MonadParams m => {- FilePath -> -} FilePath -> m ()
checkImports sourcePath =
    do source <- liftIO $ try ((,) <$> parseFileWithComments defaultParseMode sourcePath <*> readFile sourcePath)
       case source of
         Left (e :: SomeException) -> error (sourcePath ++ ": " ++ show e)
         Right ((ParseOk (m@(Module _ (ModuleName name) _ _ _ _ _), comments)), sourceText) ->
             do let importsPath = name <.> ".imports"
                result <- liftIO (parseFileWithMode (defaultParseMode {extensions = [PackageImports] ++ extensions defaultParseMode}) importsPath)
                putJunk importsPath
                case result of
                  ParseOk newImports -> updateSource newImports sourcePath m comments sourceText
                  _ -> error (importsPath ++ ": parse failed")
         Right _ -> error (sourcePath ++ ": parse failed")

removeFile' :: FilePath -> IO ()
removeFile' path = hPutStrLn stderr ("removeFile " ++ show path) >> removeFile path

-- importsPathToSourcePath :: String -> FilePath
-- importsPathToSourcePath name = map (\ c -> if c == '.' then '/' else c) (dropSuffix ".imports" name) <> ".hs"

-- sourcePathToImportsPath :: D.ModuleName -> FilePath
-- sourcePathToImportsPath moduleName = intercalate "." (D.components moduleName) <> ".imports"

-- | If all the parsing went well and the new imports differ from the
-- old, update the source file with the new imports.
updateSource :: MonadParams m => Module -> FilePath -> Module -> [Comment] -> String -> m ()
updateSource _ sourcePath (Module _ _ _ _ Nothing _ _) _ _ =
    error ("Invalid source file " ++ sourcePath ++ ": Won't modify source file with no explicit export list")
updateSource _ sourcePath (Module _ _ _ _ _ _ []) _ _ =
    error ("Invalid source file " ++ sourcePath ++ ": Won't modify source file with no declarations")
updateSource (Module _ _ _ _ _ newImports _) sourcePath (m@(Module _ _ _ _ _ oldImports _)) comments sourceText =
    maybe (liftIO (putStrLn (sourcePath ++ ": no changes")))
          (\ text ->
               liftIO (putStrLn (sourcePath ++ ": replacing imports")) >>
               replaceFile (++ "~") sourcePath text)
          (replaceImports oldImports (fixNewImports newImports) sourceText (importsSpan m comments))

-- | Final touch-ups - sort and merge similar imports.
fixNewImports :: [ImportDecl] -> [ImportDecl]
fixNewImports imports =
    map mergeDecls (groupBy ((==) `on` noSpecs) (sortBy importCompare imports))
    where
      importCompare a b =
          case (compare `on` importModule) a b of
            EQ -> (compare `on` noSpecs) a b
            x -> x
      noSpecs :: ImportDecl -> ImportDecl
      noSpecs x = x {importSpecs = fmap (\ (flag, _) -> (flag, [])) (importSpecs x)}
      mergeDecls :: [ImportDecl] -> ImportDecl
      mergeDecls xs@(x : _) = x {importSpecs = mergeSpecs (catMaybes (map importSpecs xs))}
      mergeDecls [] = error "mergeDecls"
      mergeSpecs :: [(Bool, [ImportSpec])] -> Maybe (Bool, [ImportSpec])
      mergeSpecs (x : xs) = Just (fst x, sortBy compareSpecs (nub (concat (snd x : map snd xs))))
      mergeSpecs [] = error "mergeSpecs"

compareSpecs :: ImportSpec -> ImportSpec -> Ordering
compareSpecs a b =
    case compare (map toLower $ specName a) (map toLower $ specName b) of
      EQ -> compare a b
      x -> x

{-
findSourcePath :: [FilePath] -> FilePath -> Bool -> Failing FilePath
findSourcePath exePaths path exists =
    case (exists, matches) of
      (True, []) -> Success path
      (True, _) -> Success path -- dubious
      (False, [x]) -> Success x
      (False, []) -> Failure ["Can't find " ++ path]
      (False, xs) -> Failure ["Multiple executables named " ++ path ++ ": " ++ show xs]
    where
      matches = filter (isSuffixOf ('/' : path)) exePaths
-}

dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix suf x = if isSuffixOf suf x then take (length x - length suf) x else x

-- dropPrefix :: Eq a => [a] -> [a] -> [a]
-- dropPrefix pre x = if isPrefixOf pre x then drop (length x) x else x
