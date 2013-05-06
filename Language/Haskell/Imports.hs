{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.Imports
    ( cleanDumpedImports
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try, catch, throw)
import Data.List (isSuffixOf, isInfixOf, tails, isPrefixOf, findIndex)
import Data.Monoid ((<>))
import Language.Haskell.Exts.Extension (Extension(PackageImports))
import Language.Haskell.Exts.Syntax (Module(..), ImportDecl(..))
import Language.Haskell.Exts.Parser (ParseMode(..))
import Language.Haskell.Exts (ParseResult(..), parseFile, parseFileWithMode, defaultParseMode)
import System.IO.Error (isDoesNotExistError)
import System.Directory (getDirectoryContents, removeFile, doesFileExist, renameFile)

-- | To use this function you must first make sure...
--    1. There are up-to-date .imports files in the top directory, generated when
--       the -ddump-minimal-imports flag is added to ghc-options.
--    2. The first import must be preceded by a blank line
--    3. The last import must be followed by a blank line
--    4. No imports commented out with {- -} appear anywhere
-- It will not operate on files without an explicit export list or on files with no declarations.
cleanDumpedImports :: [FilePath] -> IO ()
cleanDumpedImports exePaths =
    getDirectoryContents "." >>= mapM_ (\ path -> if isSuffixOf ".imports" path then checkImport path else return ())
    where
      checkImport importsPath =
          do result <- parseFileWithMode (defaultParseMode {extensions = [PackageImports] ++ extensions defaultParseMode}) importsPath
             case result of
               (ParseOk (Module _ _ _ _ _ newImports _)) ->
                   do putStrLn ("parsed: " ++ importsPath)
                      sourcePath <- findSourcePath (map (\ c -> if c == '.' then '/' else c) (dropSuffix ".imports" importsPath) <> ".hs")
                      importsText <- readFile importsPath
                      source <- try ((,) <$> parseFile sourcePath <*> readFile sourcePath)
                      either (\ (e :: SomeException) -> putStrLn (sourcePath ++ ": " ++ show e)) (uncurry (updateSource importsPath newImports importsText sourcePath)) source
               (ParseOk _) -> putStrLn ("ignored: " ++ importsPath)
               _ -> putStrLn ("Parse of imports failed: " ++ show result)
      findSourcePath :: FilePath -> IO FilePath
      findSourcePath path =
          doesFileExist path >>= \ exists ->
              case (exists, matches) of
                (True, []) -> return path
                (False, [x]) -> return x
                (False, []) -> error ("Can't find " ++ path)
                (_, xs) -> error ("Multiple executables named " ++ path ++ ": " ++ show xs)
          where
            matches = filter (isSuffixOf ('/' : path)) exePaths
      updateSource :: FilePath -> [ImportDecl] -> String -> FilePath -> ParseResult Module -> String -> IO ()
      updateSource importsPath newImports importsText sourcePath (ParseOk (Module _ _ _ _ (Just _) oldImports (_ : _))) sourceText =
          if newImports /= oldImports then replaceImports importsPath sourceText importsText sourcePath else return ()
      -- Assume the import section begins with a blank line and then
      -- "import" and ends with the a blank line following "\nimport"
      updateSource importsPath newImports importsText sourcePath _ sourceText =
          putStrLn ("Invalid source file: " ++ sourcePath)
      replaceImports :: FilePath -> String -> String -> FilePath -> IO ()
      replaceImports importsPath text imports sourcePath =
          let start = findIndex (isPrefixOf "\n\nimport") (tails text)
              (prefix, rest) = splitAt (maybe 0 id start + 1) text
              last = maybe 1 (+ 1) (findIndex (not . isInfixOf "\nimport") (tails rest))
              suffix = drop last rest
              after = fmap (+ 2) (findIndex (isPrefixOf "\n\n") (tails suffix)) in
          case after of
            Nothing -> putStrLn (sourcePath ++ ": no changes")
            Just after' ->
                let suffix' = drop after' suffix
                    text' = prefix ++ "\n" ++ imports ++ "\n" ++ suffix' in
                if text /= text'
                then putStrLn (sourcePath ++ ": imports replaced by " ++ importsPath) >>
                     replaceFile (++ "~") sourcePath text'
                else putStrLn (sourcePath ++ ": no changes")

replaceFile :: (FilePath -> FilePath) -> FilePath -> String -> IO ()
replaceFile backup path text =
    remove >> rename >> write
    where
      remove = removeFile (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      rename = renameFile path (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      write = writeFile path text

dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix suf x =
    if isSuffixOf suf x then take (length x - length suf) x else x

{-
-- ImportDecl { importLoc = SrcLoc {srcFilename = "<unknown>.hs", srcLine = 1, srcColumn = 1}
--            , importModule = ModuleName "Text.PrettyPrint.ANSI.Leijen"
--            , importQualified = False
--            , importSrc = False
--            , importPkg = Nothing
--            , importAs = Nothing
--            , importSpecs = Just (False,[IAbs (Ident "Doc"),IVar (Ident "text")])}

prettyImport :: ImportDecl -> Doc
prettyImport (ImportDecl { importLoc = loc
                         , importAs = as
                         , importQualified = qual
                         , importModule = ModuleName mod
                         -- , importSrc = src
                         , importPkg = pkg
                         , importSpecs = specs}) =
    text "import " <>
    maybe empty (\ name -> text (show name ++ " ")) as <>
    if qualified then text "qualified " else empty <>
    text mod <>
    maybe empty prettySpecList specs


prettySpecList :: ImportSpecList -> Doc
prettySpecList (ImportSpecList op flag specs)


prettySpecs :: [ImportSpec] -> Doc
prettySpecs specs = text "(" <> hsep (text ", ") (map prettySpecList specs) <> text ")"

prettySpec :: ImportSpec -> Doc
prettySpec (IVar name) = empty
prettySpec (IAbs Name) = empty
prettySpec (IThingAll Name) = empty
prettySpec (IThingWith Name [CName]) = empty
-}
