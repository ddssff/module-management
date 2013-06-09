{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Util.SrcLoc
    ( HasSrcSpan(..)
    , HasSrcLoc(..)
    , HasEndLoc(..)
    , srcSpanTriple
    , srcLocPairTriple
    , srcSpanText
    , srcPairText
    , untabify
    ) where

import Data.Default (def, Default)
import Data.List (groupBy, intercalate)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(..), ExportSpec(..), ExportSpecList(..), ImportDecl(ImportDecl), ModuleHead(..), ModuleName(..), ModulePragma(..), WarningText(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), srcSpanEnd, SrcSpanInfo(..), srcSpanStart)

untabify :: String -> String
untabify s =
    loop 0 s
    where
      loop :: Int -> String -> String
      loop n ('\t' : s') = replicate (8 - mod n 8) ' ' ++ loop 0 s'
      loop _ ('\n' : s') = '\n' : loop 0 s'
      loop n (c : s') = c : loop (n + 1) s'
      loop _ [] = []

-- | A version of lines that preserves the presence or absence of a
-- terminating newline
lines' :: String -> [String]
lines' s =
    -- Group characters into strings containing either only newlines or no newlines,
    -- and then transform the newline only strings into empty lines.
    bol (groupBy (\ a b -> a /= '\n' && b /= '\n') s)
    where
      -- If we are at beginning of line and see a newline, insert an empty
      bol ("\n" : xs) = "" : bol xs
      -- If we are at beginning of line and see something else, call end of line
      bol (x : xs) = x : eol xs
      -- If we see EOF at bol insert a trailing empty
      bol [] = [""]
      -- If we are seeking end of line and see a newline, go to beginning of line
      eol ("\n" : xs) = bol xs
      -- This shouldn't happen
      eol (x : xs) = x : eol xs
      eol [] = []

-- type Module = A.Module SrcSpanInfo
type ModuleHead = A.ModuleHead SrcSpanInfo
type ModulePragma = A.ModulePragma SrcSpanInfo
type ModuleName = A.ModuleName SrcSpanInfo
type WarningText = A.WarningText SrcSpanInfo
type ExportSpecList = A.ExportSpecList SrcSpanInfo
type ExportSpec = A.ExportSpec SrcSpanInfo
type ImportDecl = A.ImportDecl SrcSpanInfo
-- type ImportSpecList = A.ImportSpecList SrcSpanInfo
-- type ImportSpec = A.ImportSpec SrcSpanInfo
type Decl = A.Decl SrcSpanInfo
-- type QName = A.QName SrcSpanInfo
-- type Name = A.Name SrcSpanInfo
-- type Type = A.Type SrcSpanInfo

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

-- | Class of values that contain a source location.
class HasSrcLoc x where
    srcLoc :: x -> SrcLoc

instance HasSrcLoc SrcSpan where
    srcLoc (SrcSpan f b e _ _) = SrcLoc f b e

-- | Class of values that contain an end location of a span
class HasEndLoc x where
    endLoc :: x -> SrcLoc

instance HasEndLoc SrcSpan where
    endLoc (SrcSpan f _ _ b e) = SrcLoc f b e

instance HasEndLoc String where
    endLoc text = def {srcLine = length ls, srcColumn = length (last ls) + 1}
        where ls = lines' text

-- | Given a string and a span, return the portion of the text before
-- the span, the portion within the span, and the portion after.
srcSpanTriple :: SrcSpan -> String -> (String, String, String)
srcSpanTriple sp s =
    srcLocPairTriple srcSpanStart' srcSpanEnd' s
    where
      srcSpanStart' :: SrcLoc
      srcSpanStart' = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanStart sp)
      srcSpanEnd' :: SrcLoc
      srcSpanEnd' = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanEnd sp)

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

instance Default SrcLoc where
    def = SrcLoc "<unknown>.hs" 1 1

instance Default SrcSpanInfo where
    def = SrcSpanInfo {srcInfoSpan = def, srcInfoPoints = def}

instance Default SrcSpan where
    def = SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 1, srcSpanEndLine = 1, srcSpanStartColumn = 1, srcSpanEndColumn = 1}
