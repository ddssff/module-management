{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Common
    ( importsSpan
    , renameSpec
    , specName
    , replaceChangedImports
    ) where

import Control.Exception (catch, throw)
import Data.Monoid ((<>))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcSpan)
import Language.Haskell.Exts.Syntax (CName, ImportDecl, ImportSpec(..), Module(..), Name(..))
import Language.Haskell.Exts.Parser (parseModule)
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(..), PPLayout(..), prettyPrintWithMode)
import Language.Haskell.Exts (ParseResult(ParseOk))
import Language.Haskell.Imports.SrcSpan (cutSrcSpan, HasSrcLoc(srcLoc))
import System.Directory (removeFile, renameFile)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)

importsSpan :: Module -> SrcSpan
importsSpan (Module _ _ _ _ _ (i : _) (d : _)) = mkSrcSpan (srcLoc i) (srcLoc d)
importsSpan _ = error "importsSpan"

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
