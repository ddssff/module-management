{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Common
    ( Display(..)
    , tildeBackup
    , noBackup
    , readFileMaybe
    , replaceFile
    , replaceFileIfDifferent
    , removeFileIfPresent
    , groupBy'
    , withCurrentDirectory
    , untabify
    , lines'
    , checkParse
    , Module
    , ModuleHead
    , ModulePragma
    , ModuleName
    , WarningText
    , ExportSpecList
    , ExportSpec
    , ImportDecl
    , ImportSpecList
    , ImportSpec
    , Decl
    , Name
    , HasSrcSpan(..)
    , PutSrcSpan(..)
    , HasSrcLoc(..)
    , HasEndLoc(..)
    , textEndLoc
    , srcSpanTriple
    , srcLocPairTriple
    , srcSpanText
    , srcPairText
    , HasSymbols(symbols)
    , voidName
    , mapNames
    , ModuleResult(..)
    ) where

import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(..), DeclHead(..), ExportSpec(..), ExportSpecList(..), ImportDecl(ImportDecl), ImportSpec(..), ImportSpecList, InstHead(..), Match(..), Module, ModuleHead(..), ModuleName(..), ModulePragma(..), Name(..), QName(..), WarningText(..))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Control.Applicative ((<$>))
import Control.Exception (bracket, catch, throw)
import Data.Default (def, Default)
import Data.List (groupBy, intercalate, sortBy)
import Language.Haskell.Exts (ParseResult(ParseOk, ParseFailed))
import Language.Haskell.Exts.SrcLoc (srcSpanEnd, srcSpanStart)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..))
import System.Directory (getCurrentDirectory, removeFile, renameFile, setCurrentDirectory)
import System.IO.Error (isDoesNotExistError)

type Module = A.Module SrcSpanInfo
type ModuleHead = A.ModuleHead SrcSpanInfo
type ModulePragma = A.ModulePragma SrcSpanInfo
type ModuleName = A.ModuleName SrcSpanInfo
type WarningText = A.WarningText SrcSpanInfo
type ExportSpecList = A.ExportSpecList SrcSpanInfo
type ExportSpec = A.ExportSpec SrcSpanInfo
type ImportDecl = A.ImportDecl SrcSpanInfo
type ImportSpecList = A.ImportSpecList SrcSpanInfo
type ImportSpec = A.ImportSpec SrcSpanInfo
type Decl = A.Decl SrcSpanInfo
type Name = A.Name SrcSpanInfo

class Display a where
    display :: a -> String

tildeBackup :: FilePath -> Maybe FilePath
tildeBackup = Just . (++ "~")

noBackup :: FilePath -> Maybe FilePath
noBackup = const Nothing

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path = (Just <$> readFile path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return Nothing else throw e)

removeFileIfPresent :: FilePath -> IO ()
removeFileIfPresent path = removeFile path `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)

replaceFileIfDifferent :: FilePath -> String -> IO Bool
replaceFileIfDifferent path newText =
    do oldText <- readFileMaybe path
       if oldText == Just newText then return False else replaceFile tildeBackup path newText >> return True

-- | Replace the file at path with the given text, moving the original
-- to the location returned by passing path to backup.  If backup is
-- the identity function you're going to have a bad time.
replaceFile :: (FilePath -> Maybe FilePath) -> FilePath -> String -> IO ()
replaceFile backup path text =
    remove >> rename >> write
    where
      remove = maybe (return ()) removeFile (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      rename = maybe (return ()) (renameFile path) (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      write = writeFile path text

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path action =
    bracket (getCurrentDirectory >>= \ save -> setCurrentDirectory path >> return save)
            setCurrentDirectory
            (const action)

untabify :: String -> String
untabify s =
    loop 0 s
    where
      loop :: Int -> String -> String
      loop n ('\t' : s') = replicate (8 - mod n 8) ' ' ++ loop 0 s'
      loop _ ('\n' : s') = '\n' : loop 0 s'
      loop n (c : s') = c : loop (n + 1) s'
      loop _ [] = []

-- | Convert a compare function into an (==)
toEq :: Ord a => (a -> a -> Ordering) -> (a -> a -> Bool)
toEq cmp a b =
    case cmp a b of
      EQ -> True
      _ -> False

-- | Combine sortBy and groupBy
groupBy' :: Ord a => (a -> a -> Ordering) -> [a] -> [[a]]
groupBy' cmp xs = groupBy (toEq cmp) $ sortBy cmp xs

-- | A version of lines that preserves the presence or absence of a
-- terminating newline
lines' :: String -> [String]
lines' s =
    bol (groupBy (\ a b -> a /= '\n' && b /= '\n') s)
    where
      -- If we are at beginning of line and see a newline, insert an empty
      bol ("\n" : xs) = "" : bol xs
      -- If we are at beginning of line and see something else, call end of line
      bol (x : xs) = x : eol xs
      -- If we see EOF at bol insert a trailing empty
      bol [] = [""]
      -- If we are at end of line and see a newline, go to beginning of line
      eol ("\n" : xs) = bol xs
      -- This shouldn't happen
      eol (x : xs) = x : eol xs
      eol [] = []

checkParse :: S.ModuleName -> ParseResult a -> a
checkParse (S.ModuleName name) (ParseFailed loc msg) = throw $ userError $ "Parse Failure in " ++ name ++ " at " ++ show loc ++ ": " ++ msg
checkParse _ (ParseOk x) = x

class HasSrcSpan a where
    srcSpan :: a -> SrcSpan

instance HasSrcSpan ModuleHead where
    srcSpan (A.ModuleHead x _ _ _) = srcSpan x

instance HasSrcSpan ModuleName where
    srcSpan (A.ModuleName x _) = srcSpan x

instance HasSrcSpan ModulePragma where
    srcSpan (A.LanguagePragma x _) = srcSpan x
    srcSpan (A.OptionsPragma x _ _) = srcSpan x
    srcSpan (A.AnnModulePragma x _) = srcSpan x

instance HasSrcSpan WarningText where
    srcSpan (A.WarnText x _) = srcSpan x
    srcSpan (A.DeprText x _) = srcSpan x

instance HasSrcSpan ExportSpecList where
    srcSpan (A.ExportSpecList x _) = srcSpan x

instance HasSrcSpan ExportSpec where
    srcSpan (A.EVar x _) = srcSpan x
    srcSpan (A.EAbs x _) = srcSpan x
    srcSpan (A.EThingAll x _) = srcSpan x
    srcSpan (A.EThingWith x _ _) = srcSpan x
    srcSpan (A.EModuleContents x _) = srcSpan x

instance HasSrcSpan SrcSpanInfo where
    srcSpan = srcInfoSpan

instance HasSrcSpan SrcSpan where
    srcSpan x = x

instance HasSrcSpan ImportDecl where
    srcSpan (A.ImportDecl x _ _ _ _ _ _) = srcSpan x

instance HasSrcSpan Decl where
    srcSpan (A.TypeDecl l _ _) = srcSpan l
    srcSpan (A.TypeFamDecl l _ _) = srcSpan l
    srcSpan (A.DataDecl l _ _ _ _ _) = srcSpan l
    srcSpan (A.GDataDecl l _ _ _ _ _ _) = srcSpan l
    srcSpan (A.DataFamDecl l _ _ _) = srcSpan l
    srcSpan (A.TypeInsDecl l _ _) = srcSpan l
    srcSpan (A.DataInsDecl l _ _ _ _) = srcSpan l
    srcSpan (A.GDataInsDecl l _ _ _ _ _) = srcSpan l
    srcSpan (A.ClassDecl l _ _ _ _) = srcSpan l
    srcSpan (A.InstDecl l  _ _ _) = srcSpan l
    srcSpan (A.DerivDecl l _ _) = srcSpan l
    srcSpan (A.InfixDecl l _ _ _) = srcSpan l
    srcSpan (A.DefaultDecl l _) = srcSpan l
    srcSpan (A.SpliceDecl l _) = srcSpan l
    srcSpan (A.TypeSig l _ _) = srcSpan l
    srcSpan (A.FunBind l _) = srcSpan l
    srcSpan (A.PatBind l _ _ _ _) = srcSpan l
    srcSpan (A.ForImp l _ _ _ _ _) = srcSpan l
    srcSpan (A.ForExp l _ _ _ _) = srcSpan l
    srcSpan (A.RulePragmaDecl l _) = srcSpan l
    srcSpan (A.DeprPragmaDecl l _) = srcSpan l
    srcSpan (A.WarnPragmaDecl l _) = srcSpan l
    srcSpan (A.InlineSig l _ _ _) = srcSpan l
    srcSpan (A.InlineConlikeSig l _ _) = srcSpan l
    srcSpan (A.SpecSig l _ _) = srcSpan l
    srcSpan (A.SpecInlineSig l _ _ _ _) = srcSpan l
    srcSpan (A.InstSig l _ _) = srcSpan l
    srcSpan (A.AnnPragma l _) = srcSpan l

class PutSrcSpan a where
    putSrcSpan :: SrcSpan -> a -> a

instance PutSrcSpan SrcSpanInfo where
    putSrcSpan sp x = x {srcInfoSpan = sp} -- what should we do about srcInfoPoints?

instance PutSrcSpan ModuleHead where
    putSrcSpan sp (A.ModuleHead x a b c) = A.ModuleHead (putSrcSpan sp x) a b c

instance PutSrcSpan ModuleName where
    putSrcSpan sp (A.ModuleName x a) = A.ModuleName (putSrcSpan sp x) a

instance PutSrcSpan WarningText where
    putSrcSpan sp (A.WarnText x a) = A.WarnText (putSrcSpan sp x) a
    putSrcSpan sp (A.DeprText x a) = A.DeprText (putSrcSpan sp x) a

instance PutSrcSpan ExportSpecList where
    putSrcSpan sp (A.ExportSpecList x a) = A.ExportSpecList (putSrcSpan sp x) a

instance PutSrcSpan ExportSpec where
    putSrcSpan sp (A.EVar x a) = A.EVar (putSrcSpan sp x) a
    putSrcSpan sp (A.EAbs x a) = A.EAbs (putSrcSpan sp x) a
    putSrcSpan sp (A.EThingAll x a) = A.EThingAll (putSrcSpan sp x) a
    putSrcSpan sp (A.EThingWith x a b) = A.EThingWith (putSrcSpan sp x) a b
    putSrcSpan sp (A.EModuleContents x a) = A.EModuleContents (putSrcSpan sp x) a

instance PutSrcSpan ModulePragma where
    putSrcSpan sp (A.LanguagePragma l a) = A.LanguagePragma (putSrcSpan sp l) a
    putSrcSpan sp (A.OptionsPragma l a b) = A.OptionsPragma (putSrcSpan sp l) a b
    putSrcSpan sp (A.AnnModulePragma l a) = A.AnnModulePragma (putSrcSpan sp l) a

instance PutSrcSpan Comment where
    putSrcSpan sp (Comment a _ b) = Comment a sp b

instance PutSrcSpan ImportDecl where
    putSrcSpan sp (A.ImportDecl x a b c d e f) = A.ImportDecl (putSrcSpan sp x) a b c d e f

-- | Class of values that contain a source location.
class HasSrcLoc x where
    srcLoc :: x -> SrcLoc

instance HasSrcLoc SrcSpan where
    srcLoc (SrcSpan f b e _ _) = SrcLoc f b e

{-
instance HasSrcSpan a => HasSrcLoc a where
    srcLoc = srcLoc . srcSpan
-}

-- | Class of values that contain a source location.
class HasEndLoc x where
    endLoc :: x -> SrcLoc

instance HasEndLoc SrcSpan where
    endLoc (SrcSpan f _ _ b e) = SrcLoc f b e

{-
instance HasSrcSpan a => HasEndLoc a where
    endLoc = endLoc . srcSpan
-}

textEndLoc :: String -> SrcLoc
textEndLoc text = def {srcLine = length ls, srcColumn = length (last ls) + 1}
    where
      ls = lines' text

srcSpanTriple :: SrcSpan -> String -> (String, String, String)
srcSpanTriple sp s = srcLocPairTriple (srcSpanStart' sp) (srcSpanEnd' sp) s

srcLocPairTriple :: SrcLoc -> SrcLoc -> String -> (String, String, String)
srcLocPairTriple b e s =
    let (bmtext, etext) = cutSrcLoc e s
        (btext, mtext) = cutSrcLoc b bmtext in
    (btext, mtext, etext)

srcSpanText :: SrcSpan -> String -> String
srcSpanText sp s = let (_, m, _) = srcSpanTriple sp s in m

srcPairText :: SrcLoc -> SrcLoc -> String -> String
srcPairText b e s = let (_, m, _) = srcLocPairTriple b e s in m

-- | Split text into two regions, before and after the given location
cutSrcLoc :: SrcLoc -> String -> (String, String)
cutSrcLoc loc s =
    case splitAt (srcLine loc - 1) (lines' s) of
      (beforeLines, lastLine : afterLines) ->
          case splitAt (srcColumn loc - 1) lastLine of
            ("", "") -> (unlines beforeLines, intercalate "\n" afterLines)
            (startOfLine, "") -> (intercalate "\n" (beforeLines ++ [startOfLine]), intercalate "\n" ("" : afterLines))
            ("", endOfLine) -> (unlines beforeLines, intercalate "\n" ([endOfLine] ++ afterLines))
            (startOfLine, endOfLine) -> (intercalate "\n" (beforeLines ++ [startOfLine]), intercalate "\n" ([endOfLine] ++ afterLines))
      (beforeLines, []) -> (intercalate "\n" beforeLines, "")

srcSpanStart' :: SrcSpan -> SrcLoc
srcSpanStart' sp = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanStart sp)

srcSpanEnd' :: SrcSpan -> SrcLoc
srcSpanEnd' sp = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanEnd sp)

instance Default SrcLoc where
    def = SrcLoc "<unknown>.hs" 1 1

instance Default SrcSpanInfo where
    def = SrcSpanInfo {srcInfoSpan = def, srcInfoPoints = def}

instance Default SrcSpan where
    def = SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 1, srcSpanEndLine = 1, srcSpanStartColumn = 1, srcSpanEndColumn = 1}

class HasSymbols a where
    symbols :: a -> [A.Name ()]

instance HasSymbols (A.Decl a) where
    symbols (A.TypeDecl _ x _) = symbols x
    symbols (A.TypeFamDecl _ x _) = symbols x
    symbols (A.DataDecl _ _ _ x _ _) = symbols x
    symbols (A.GDataDecl _ _ _ x _ _ _) = symbols x
    symbols (A.DataFamDecl _ _ x _) = symbols x
    symbols (A.TypeInsDecl _ _ _) = error "TypeInsDecl"
    symbols (A.DataInsDecl _ _ _ _ _) = error "DataInsDecl"
    symbols (A.GDataInsDecl _ _ _ _ _ _) = error "GDataInsDecl"
    symbols (A.ClassDecl _ _ x _ _) = symbols x
    symbols (A.InstDecl _ _ x _) = symbols x
    symbols (A.DerivDecl _ _ x) = symbols x
    symbols (A.InfixDecl _ _ _ _) = error "InfixDecl"
    symbols (A.DefaultDecl _ _) = error "DefaultDecl"
    symbols (A.SpliceDecl _ _) = error "SpliceDecl"
    symbols (A.TypeSig _ x _) = concatMap symbols x
    symbols (A.FunBind _ matches) = concatMap symbols matches
    symbols (A.PatBind _ _ _ _ _) = error "PatBind"
    symbols (A.ForImp _ _ _ _ _ _) = error "ForImp"
    symbols (A.ForExp _ _ _ _ _) = error "ForExp"
    symbols (A.RulePragmaDecl _ _) = error "RulePragmaDecl"
    symbols (A.DeprPragmaDecl _ _) = error "DeprPragmaDecl"
    symbols (A.WarnPragmaDecl _ _) = error "WarnPragmaDecl"
    symbols (A.InlineSig _ _ _ x) = symbols x
    symbols (A.InlineConlikeSig _ _ x) = symbols x
    symbols (A.SpecSig _ x _) = symbols x
    symbols (A.SpecInlineSig _ _ _ x _) = symbols x
    symbols (A.InstSig _ _ x) = symbols x
    symbols (A.AnnPragma _ _) = error "AnnPragma"

instance HasSymbols (A.DeclHead a) where
    symbols (A.DHead _ x _) = symbols x
    symbols (A.DHInfix _ _ x _) = symbols x
    symbols (A.DHParen _ x) = symbols x

instance HasSymbols (A.InstHead a) where
    symbols (A.IHead _ x _) = symbols x
    symbols (A.IHInfix _ _ x _) = symbols x
    symbols (A.IHParen _ x) = symbols x

instance HasSymbols (A.QName a) where
    symbols (A.Qual _ _ x) = symbols x
    symbols (A.UnQual _ x) = symbols x
    symbols (A.Special _ _) = error "Special"

instance HasSymbols (A.Name a) where
    symbols (A.Ident _ x) = [A.Ident () x]
    symbols (A.Symbol _ x) = [A.Symbol () x]

instance HasSymbols (A.Match a) where
    symbols (A.Match _ x _ _ _) = symbols x
    symbols (A.InfixMatch _ _ x _ _ _) = symbols x

instance HasSymbols ImportSpec where
    symbols (A.IVar _ name) = symbols name
    symbols (A.IAbs _ name) = symbols name
    symbols (A.IThingAll _ name) = symbols name
    symbols (A.IThingWith _ name _) = symbols name

voidName :: A.Name a -> A.Name ()
voidName (A.Ident _ x) = A.Ident () x
voidName (A.Symbol _ x) = A.Symbol () x

mapNames :: Default a => [A.Name ()] -> [A.Name a]
mapNames [] = []
mapNames (A.Ident () x : more) = A.Ident def x : mapNames more
mapNames (A.Symbol () x : more) = A.Symbol def x : mapNames more

data ModuleResult
    = Unchanged S.ModuleName
    | Removed S.ModuleName
    | Modified S.ModuleName String
    deriving (Show, Eq, Ord)
