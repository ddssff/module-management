{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports
    ( cleanDumpedImports
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Applicative.Error (Failing(..))
import Control.Exception (SomeException, try, catch, throw)
import Data.List (isSuffixOf, isInfixOf, tails, isPrefixOf, findIndex, intercalate)
import Data.Monoid ((<>))
import Distribution.PackageDescription (PackageDescription (executables), Executable(modulePath))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import Language.Haskell.Exts.Extension (Extension(PackageImports))
import Language.Haskell.Exts.Syntax (Module(..), ImportDecl(..))
import Language.Haskell.Exts.Parser (ParseMode(..))
import Language.Haskell.Exts (ParseResult(..), parseFile, parseFileWithMode, defaultParseMode)
import Prelude hiding (last)
import System.IO.Error (isDoesNotExistError)
import System.Directory (getDirectoryContents, removeFile, doesFileExist, renameFile)

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
cleanDumpedImports :: LocalBuildInfo -> IO ()
cleanDumpedImports lbi = cleanDumpedImports' (map modulePath (executables (localPkgDescr lbi)))

cleanDumpedImports' :: [FilePath] -> IO ()
cleanDumpedImports' exePaths =
    getDirectoryContents "." >>= mapM_ (\ path -> if isSuffixOf ".imports" path then checkImport exePaths path else return ())

checkImport :: [FilePath] -> FilePath -> IO ()
checkImport exePaths importsPath =
    do result <- parseFileWithMode (defaultParseMode {extensions = [PackageImports] ++ extensions defaultParseMode}) importsPath
       case result of
         (ParseOk (Module _ _ _ _ _ newImports _)) ->
             do sourcePath <- findSourcePath exePaths (moduleToPath importsPath)
                case sourcePath of
                  Failure messages -> putStrLn (intercalate ", " messages)
                  Success sourcePath' ->
                      do importsText <- readFile importsPath
                         source <- try ((,) <$> parseFile sourcePath' <*> readFile sourcePath')
                         either (\ (e :: SomeException) -> putStrLn (sourcePath' ++ ": " ++ show e)) (uncurry (updateSource newImports importsText sourcePath')) source
         _ -> putStrLn ("Parse of imports failed: " ++ show result)

moduleToPath :: String -> FilePath
moduleToPath name =
    map (\ c -> if c == '.' then '/' else c) (dropSuffix ".imports" name) <> ".hs"

findSourcePath :: [FilePath] -> FilePath -> IO (Failing FilePath)
findSourcePath exePaths path = doesFileExist path >>= return . findSourcePath' exePaths path

updateSource :: [ImportDecl] -> String -> FilePath -> ParseResult Module -> String -> IO ()
updateSource newImports importsText sourcePath (ParseOk (Module _ _ _ _ (Just _) oldImports (_ : _))) sourceText =
    if newImports /= oldImports then replaceImports sourceText importsText sourcePath else return ()
updateSource _ _ sourcePath _ _ =
    putStrLn ("Invalid source file: " ++ sourcePath)

replaceImports :: String -> String -> FilePath -> IO ()
replaceImports text imports sourcePath =
    maybe (return ()) (replaceFile (++ "~") sourcePath) (replaceImports' text imports)

-- | If backup is the identity function you're going to have a bad time.
replaceFile :: (FilePath -> FilePath) -> FilePath -> String -> IO ()
replaceFile backup path text =
    remove >> rename >> write
    where
      remove = removeFile (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      rename = renameFile path (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      write = writeFile path text

findSourcePath' :: [FilePath] -> FilePath -> Bool -> Failing FilePath
findSourcePath' exePaths path exists =
    case (exists, matches) of
      (True, []) -> Success path
      (True, _) -> Success path -- dubious
      (False, [x]) -> Success x
      (False, []) -> Failure ["Can't find " ++ path]
      (False, xs) -> Failure ["Multiple executables named " ++ path ++ ": " ++ show xs]
    where
      matches = filter (isSuffixOf ('/' : path)) exePaths

-- Assume the import section begins with a blank line and then
-- "import" and ends with the a blank line following "\nimport"
replaceImports' :: String -> String -> Maybe String
replaceImports' text imports =
    let start = findIndex (isPrefixOf "\n\nimport") (tails text)
        (prefix, rest) = splitAt (maybe 0 id start + 1) text
        last = maybe 1 (+ 1) (findIndex (not . isInfixOf "\nimport") (tails rest))
        suffix = drop last rest
        after = fmap (+ 2) (findIndex (isPrefixOf "\n\n") (tails suffix)) in
    case after of
      Nothing -> Nothing
      Just after' ->
          let suffix' = drop after' suffix
              text' = prefix ++ "\n" ++ imports ++ "\n" ++ suffix' in
          if text /= text'
          then Just text'
          else Nothing

dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix suf x =
    if isSuffixOf suf x then take (length x - length suf) x else x
