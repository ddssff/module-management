{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Clean
    ( cleanImports
    , moveImports
    , cleanBuildImports
    , test1
    , test2
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Applicative.Error (Failing(..))
import Control.Exception (catch, SomeException, throw, try)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (findIndex, groupBy, intercalate, isSuffixOf, nub, sortBy, tails)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>))
import Distribution.PackageDescription (Executable(modulePath), PackageDescription(executables))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import Language.Haskell.Exts.Extension (Extension(PackageImports))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcSpan)
import Language.Haskell.Exts.Syntax (CName, ImportDecl(..), ImportSpec(..), Module(..), ModuleName(ModuleName), Name(Ident, Symbol))
import Language.Haskell.Exts.Parser (ParseMode(..), parseModule)
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(..), PPLayout(..), prettyPrintWithMode)
import Language.Haskell.Exts (defaultParseMode, parseFile, parseFileWithMode, ParseResult(..))
import Language.Haskell.Imports.SrcSpan (cutSrcSpan, HasSrcLoc(srcLoc))
import System.Directory (doesFileExist, getDirectoryContents, removeFile, renameFile)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Process (readProcessWithExitCode, showCommandForUser)

type FQID = String -- ^ Fully qualified identifier - e.g. Language.Haskell.Imports.Clean.cleanImports

-- | Clean up the imports of a source file.
cleanImports :: Bool -> FilePath -> IO ()
cleanImports dryRun sourcePath =
    (dump >> replace >> cleanup) `catch` (\ (e :: SomeException) -> hPutStrLn stderr (show e))
    where
      importsPath = (sourcePathToImportsPath sourcePath)

      dump = let cmd = "ghc"
                 args = ["--make", "-ddump-minimal-imports", sourcePath] in
             readProcessWithExitCode cmd args "" >>= \ (code, _out, err) ->
             case code of
               ExitSuccess -> return ()
               ExitFailure _ -> error (sourcePath ++ ": compile failed\n " ++ showCommandForUser cmd args ++ " ->\n" ++ err)

      replace = checkImports dryRun importsPath sourcePath

      cleanup = removeFile (dropSuffix ".hs" sourcePath <> ".o") >>
                removeFile (dropSuffix ".hs" sourcePath <> ".hi") >>
                removeFile importsPath

-- | This function needs to be able to compile the source file, so it
-- must be run *before* the declaration actually gets moved to its
-- new module.
moveImports :: Bool -> [(FQID, FQID)] -> FilePath -> IO ()
moveImports dryRun moves sourcePath =
    do source <- try ((,) <$> parseFile sourcePath <*> readFile sourcePath)
       case source of
         Left (e :: SomeException) -> error (sourcePath ++ ": " ++ show e)
         Right (ParseOk m@(Module _ _ _ _ _ oldImports _), sourceText) ->
             replaceChangedImports dryRun oldImports (doMoves moves oldImports) sourceText sourcePath (importsSpan m)
         Right _ -> error (sourcePath ++ ": could not parse")

doMoves :: [(FQID, FQID)] -> [ImportDecl] -> [ImportDecl]
doMoves moves imports =
    foldr moveDecls [] moves
    where
      moveDecls :: (FQID, FQID) -> [ImportDecl] -> [ImportDecl]
      moveDecls (src, dst) decls =
          foldr moveDecl decls imports
          where
            (srcM, srcN) = parseFQID src
            (dstM, dstN) = parseFQID dst
            moveDecl :: ImportDecl -> [ImportDecl] -> [ImportDecl]
            moveDecl decl@(ImportDecl {importSpecs = Nothing}) result = decl : result
            moveDecl decl@(ImportDecl {importModule = m, importSpecs = Just (flag, specs)}) result =
                [decl {importSpecs = Just (flag, specs')}] ++ decls' ++ result
                where
                  (specs', decls') = foldr moveSpec ([], []) specs
                  moveSpec :: ImportSpec -> ([ImportSpec], [ImportDecl]) -> ([ImportSpec], [ImportDecl])
                  moveSpec spec (specs'', decls'') =
                      if m == srcM && spec == srcSpec
                      then (specs'', (decl {importModule = dstM, importSpecs = Just (False, [dstSpec])} : decls''))
                      else (spec : specs'', decls'')
                      where
                        srcSpec = renameSpec srcN spec
                        dstSpec = renameSpec dstN spec

renameSpec :: String -> ImportSpec -> ImportSpec
renameSpec s x = mapSpecName (const s) x

-- reName :: String -> Name -> Name
-- reName s x = mapName (const s) x

mapSpecName :: (String -> String) -> ImportSpec -> ImportSpec
mapSpecName f = foldSpec (IVar . mapName f) (IAbs . mapName f) (IThingAll . mapName f) (\ n cn -> IThingWith (mapName f n) cn)

mapName :: (String -> String) -> Name -> Name
mapName f = foldName (Ident . f) (Symbol . f)

foldSpec :: (Name -> a) -> (Name -> a) -> (Name -> a) -> (Name -> [CName] -> a) -> ImportSpec -> a
foldSpec iVar _ _ _ (IVar n) = iVar n
foldSpec _ iAbs _ _ (IAbs n) = iAbs n
foldSpec _ _ iThingAll _ (IThingAll n) = iThingAll n
foldSpec _ _ _ iThingWith (IThingWith n cn) = iThingWith n cn

foldName :: (String -> a) -> (String -> a) -> Name -> a
foldName ident _ (Ident s) = ident s
foldName _ symbol (Symbol s) = symbol s

specName :: ImportSpec -> String
specName = foldSpec (foldName id id) (foldName id id) (foldName id id) (\ n _ -> foldName id id n)

parseFQID :: FQID -> (ModuleName, String)
parseFQID s =
    let (m, n) = splitAt (fromJust (findIndex (not . elem '.') (tails s)) - 1) s in
    (ModuleName m, (tail n))

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
cleanBuildImports :: Bool -> LocalBuildInfo -> IO ()
cleanBuildImports dryRun lbi = cleanDumpedImports dryRun (map modulePath (executables (localPkgDescr lbi)))

cleanDumpedImports :: Bool -> [FilePath] -> IO ()
cleanDumpedImports dryRun exePaths =
    getDirectoryContents "." >>= mapM_ doImports
    where
      doImports importsPath | not (isSuffixOf ".imports" importsPath) = return ()
      doImports importsPath =
          do let sourcePath = importsPathToSourcePath importsPath
             result <- doesFileExist sourcePath >>= return . findSourcePath exePaths sourcePath
             case result of
               Failure messages -> putStrLn (intercalate ", " messages)
               Success sourcePath' ->
                   try (checkImports dryRun importsPath sourcePath') >>=
                   either (\ (e :: SomeException) -> putStrLn (show e)) return

-- | Parse the import list generated by GHC, parse the original source
-- file, and if all goes well insert the new imports into the old
-- source file.
checkImports :: Bool -> FilePath -> FilePath -> IO ()
checkImports dryRun importsPath sourcePath =
    do result <- parseFileWithMode (defaultParseMode {extensions = [PackageImports] ++ extensions defaultParseMode}) importsPath
       case result of
         ParseOk newImports ->
             do source <- try ((,) <$> parseFile sourcePath <*> readFile sourcePath)
                either (\ (e :: SomeException) -> error (sourcePath ++ ": " ++ show e))
                       (uncurry (updateSource dryRun newImports sourcePath)) source
         _ -> error ("Parse of imports failed: " ++ show result)

importsPathToSourcePath :: String -> FilePath
importsPathToSourcePath name = map (\ c -> if c == '.' then '/' else c) (dropSuffix ".imports" name) <> ".hs"

sourcePathToImportsPath :: FilePath -> FilePath
sourcePathToImportsPath sourcePath = map (\ c -> if c == '/' then '.' else c) (dropSuffix ".hs" sourcePath) <> ".imports"

-- | If all the parsing went well and the new imports differ from the
-- old, update the source file with the new imports.
updateSource :: Bool -> Module -> FilePath -> ParseResult Module -> String -> IO ()
updateSource _ _ sourcePath (ParseOk (Module _ _ _ _ Nothing _ _)) _ =
    error ("Invalid source file " ++ sourcePath ++ ": Won't modify source file with no explicit export list")
updateSource _ _ sourcePath (ParseOk (Module _ _ _ _ _ _ [])) _ =
    error ("Invalid source file " ++ sourcePath ++ ": Won't modify source file with no declarations")
updateSource dryRun (Module _ _ _ _ _ newImports _) sourcePath (ParseOk m@(Module _ _ _ _ _ oldImports _)) sourceText =
    replaceChangedImports dryRun oldImports (fixNewImports newImports) sourceText sourcePath (importsSpan m)
updateSource _ _ sourcePath (ParseFailed _ _) _ = error (sourcePath ++ ": could not parse")

importsSpan :: Module -> SrcSpan
importsSpan (Module _ _ _ _ _ (i : _) (d : _)) = mkSrcSpan (srcLoc i) (srcLoc d)
importsSpan _ = error "importsSpan"

-- | Final touch-ups - sort and merge similar imports.
fixNewImports :: [ImportDecl] -> [ImportDecl]
fixNewImports imports =
    map mergeDecls (groupBy ((==) `on` noSpecs) (sortBy (compare `on` noSpecs) imports))
    where
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

replaceChangedImports :: Bool -> [ImportDecl] -> [ImportDecl] -> String -> FilePath -> SrcSpan -> IO ()
replaceChangedImports dryRun oldImports newImports sourceText sourcePath importsSpan =
    if newPretty /= oldPretty -- the ImportDecls won't match because they have different SrcLoc values
    then hPutStrLn stderr (sourcePath ++ ": replacing imports -\n" ++ oldPretty ++ "\n ->\n" ++ newPretty) >>
         replaceImports dryRun newImports sourceText sourcePath importsSpan
    else putStrLn (sourcePath ++ ": no changes")
    where
      oldPretty = prettyImports oldImports
      newPretty = prettyImports newImports

replaceImports :: Bool -> [ImportDecl] -> String -> FilePath -> SrcSpan -> IO ()
replaceImports dryRun newImports sourceText sourcePath importsSpan =
    maybe (return ())
          (\ text -> replaceFile dryRun (++ "~") sourcePath text)
          (replaceImports' sourceText importsSpan (prettyImports newImports))

prettyImports :: [ImportDecl] -> String
prettyImports imports =
    munge . prettyPrintWithMode (defaultMode {layout = PPInLine}) $ Module a b c d e imports f
    where
      ParseOk (Module a b c d e _ f) = parseModule ""
      -- Strip of the module declaration line, the leading spaces, and the terminating semicolons
      munge = unlines . map (init . tail . tail) . tail . lines

-- | If backup is the identity function you're going to have a bad time.
replaceFile :: Bool -> (FilePath -> FilePath) -> FilePath -> String -> IO ()
replaceFile True _ path new =
    putStrLn ("dryRun: replaceFile " ++ show path ++ " " ++ show new)
replaceFile _ backup path text =
    remove >> rename >> write
    where
      remove = removeFile (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      rename = renameFile path (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      write = writeFile path text

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

-- Assume the import section begins with a blank line and then
-- "import" and ends with the a blank line following "\nimport"
replaceImports' :: String -> SrcSpan -> String -> Maybe String
replaceImports' old sp imports =
    let (hd, tl) = cutSrcSpan sp old
        -- Instead of inserting this newline we should figure out what was
        -- between the last import and the first declaration, but not sure
        -- how to locate the end of an import.
        new = hd <> imports <> "\n" <> tl in
    if new /= old then Just new else Nothing
{-
    let start = findIndex (isPrefixOf "\n\nimport ") (tails text)
        (prefix, rest) = splitAt (maybe 0 id start + 1) text
        final = maybe 1 (+ 1) (findIndex (not . isInfixOf "\nimport ") (tails rest))
        suffix = drop final rest
        after = fmap (+ 2) (findIndex (isPrefixOf "\n\n") (tails suffix)) in
    case after of
      Nothing -> Nothing
      Just after' ->
          let suffix' = drop after' suffix
              text' = prefix ++ "\n" ++ imports ++ "\n" ++ suffix' in
          if text /= text'
          then Just text'
          else Nothing
-}

dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix suf x = if isSuffixOf suf x then take (length x - length suf) x else x

-- dropPrefix :: Eq a => [a] -> [a] -> [a]
-- dropPrefix pre x = if isPrefixOf pre x then drop (length x) x else x

test1 :: IO ()
test1 = cleanImports True "Language/Haskell/Imports/Clean.hs"
test2 :: IO ()
test2 = moveImports True [("Language.Haskell.Imports.Clean.moveImports", "Language.Haskell.Imports.Move.moveImports")] "Language/Haskell/Imports/Clean.hs"
