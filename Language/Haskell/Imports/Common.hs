{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Common
    ( HasSrcLoc(srcLoc)
    , importsSpan
    , renameSpec
    , specName
    , replaceImports
    , replaceFile
    ) where

import Control.Exception (catch, throw)
import Data.Monoid ((<>))
import Language.Haskell.Exts.SrcLoc (mkSrcSpan, SrcSpan)
import Language.Haskell.Exts.Syntax (CName, ImportDecl, ImportSpec(..), Module(..), Name(..))
import Language.Haskell.Exts.Parser (parseModule)
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(..), PPLayout(..), prettyPrintWithMode)
import Language.Haskell.Exts (ParseResult(ParseOk))
import System.Directory (removeFile, renameFile)
import System.IO.Error (isDoesNotExistError)
import Data.List (intercalate)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), srcSpanEnd, srcSpanStart)
import Language.Haskell.Exts.Syntax (Decl(..), ImportDecl(..))

class HasSrcLoc x where
    srcLoc :: x -> SrcLoc

instance HasSrcLoc Decl where
    srcLoc (TypeDecl s _ _ _) = s
    srcLoc (TypeFamDecl s _ _ _) = s
    srcLoc (DataDecl s _ _ _ _ _ _) = s
    srcLoc (GDataDecl s _ _ _ _ _ _ _) = s
    srcLoc (DataFamDecl s _ _ _ _) = s
    srcLoc (TypeInsDecl s _ _) = s
    srcLoc (DataInsDecl s _ _ _ _) = s
    srcLoc (GDataInsDecl s _ _ _ _ _) = s
    srcLoc (ClassDecl s _ _ _ _ _) = s
    srcLoc (InstDecl s _ _ _ _) = s
    srcLoc (DerivDecl s _ _ _) = s
    srcLoc (InfixDecl s _ _ _) = s
    srcLoc (DefaultDecl s _) = s
    srcLoc (SpliceDecl s _) = s
    srcLoc (TypeSig s _ _) = s
    srcLoc (FunBind _) = error "srcLoc FunBind"
    srcLoc (PatBind s _ _ _ _) = s
    srcLoc (ForImp s _ _ _ _ _) = s
    srcLoc (ForExp s _ _ _ _) = s
    srcLoc (RulePragmaDecl s _) = s
    srcLoc (DeprPragmaDecl s _) = s
    srcLoc (WarnPragmaDecl s _) = s
    srcLoc (InlineSig s _ _ _) = s
    srcLoc (InlineConlikeSig s _ _) = s
    srcLoc (SpecSig s _ _) = s
    srcLoc (SpecInlineSig s _ _ _ _) = s
    srcLoc (InstSig s _ _ _) = s
    srcLoc (AnnPragma s _) = s

instance HasSrcLoc ImportDecl where
    srcLoc = importLoc

srcSpanStart' :: SrcSpan -> SrcLoc
srcSpanStart' sp = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanStart sp)

srcSpanEnd' :: SrcSpan -> SrcLoc
srcSpanEnd' sp = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanEnd sp)

cutSrcLoc :: SrcLoc -> String -> (String, String)
cutSrcLoc loc s =
    let (h, (t1 : t2)) = splitAt (srcLine loc - 1) (lines s) in
    let (h', t') = splitAt (srcColumn loc - 1) t1 in
    (intercalate "\n" (h ++ [h']), unlines ([t'] ++ t2))

cutSrcSpan :: SrcSpan -> String -> (String, String)
cutSrcSpan sp s =
    let (b, _) = cutSrcLoc (srcSpanStart' sp) s
        (_, e) = cutSrcLoc (srcSpanEnd' sp) s in
    (b, e)

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

-- | Compare the old and new import sets and if they differ clip out
-- the imports from the sourceText and insert the new ones.
replaceImports :: [ImportDecl] -> [ImportDecl] -> String -> SrcSpan -> Maybe String
replaceImports oldImports newImports sourceText importsSpan =
    if newPretty /= oldPretty -- the ImportDecls won't match because they have different SrcLoc values
    then let (hd, tl) = cutSrcSpan importsSpan sourceText
             -- Instead of inserting this newline we should figure out what was
             -- between the last import and the first declaration, but not sure
             -- how to locate the end of an import.
             new = hd <> newPretty <> "\n" <> tl in
         if new /= sourceText then Just new else Nothing
    else Nothing
    where
      oldPretty = prettyImports oldImports
      newPretty = prettyImports newImports

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
