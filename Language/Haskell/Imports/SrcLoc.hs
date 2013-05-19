{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.SrcLoc
    ( HasSrcLoc(..)
    , HasEndLoc(..)
    , srcSpanStart'
    , srcSpanEnd'
    , srcSpan
    , textEndLoc
    , srcSpanTriple
    , srcLocPairTriple
    , srcSpanText
    , srcPairText
    , srcLocSucc
    , untabify
    , lines'
    , tests
    ) where

import Data.Default (Default, def)
import Data.List (intercalate, groupBy)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), srcSpanEnd, srcSpanStart)
import Language.Haskell.Exts.Syntax (ImportDecl(..), Module(..), Match(..), Decl(..))
import Test.HUnit

instance Default SrcLoc where
    def = SrcLoc "<unknown>.hs" 1 1

-- | Class of values that contain a source location.
class HasSrcLoc x where
    srcLoc :: x -> SrcLoc

instance HasSrcLoc SrcSpan where
    srcLoc (SrcSpan f b e _ _) = SrcLoc f b e

instance HasSrcLoc Module where
    srcLoc (Module s _ _ _ _ _ _) = s

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
    srcLoc (FunBind (x:_)) = srcLoc x
    srcLoc (FunBind []) = error "srcLoc FunBind []"
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

instance HasSrcLoc Match where
    srcLoc (Match x _ _ _ _ _) = x

instance HasSrcLoc ImportDecl where
    srcLoc = importLoc

instance HasSrcLoc Comment where
    srcLoc (Comment _ sp _) = srcSpanStart' sp

-- | Class of values that contain a source location.
class HasEndLoc x where
    endLoc :: x -> SrcLoc

instance HasEndLoc SrcSpan where
    endLoc (SrcSpan f _ _ b e) = SrcLoc f b e

instance HasEndLoc Comment where
    endLoc (Comment _ sp _) = srcSpanEnd' sp

untabify :: String -> String
untabify s =
    loop 0 s
    where
      loop :: Int -> String -> String
      loop n ('\t' : s') = replicate (8 - mod n 8) ' ' ++ loop 0 s'
      loop _ ('\n' : s') = '\n' : loop 0 s'
      loop n (c : s') = c : loop (n + 1) s'
      loop _ [] = []

textEndLoc :: String -> SrcLoc
textEndLoc text = def {srcLine = length (lines text), srcColumn = length (last (lines text)) + 1}

srcSpanStart' :: SrcSpan -> SrcLoc
srcSpanStart' sp = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanStart sp)

srcSpanEnd' :: SrcSpan -> SrcLoc
srcSpanEnd' sp = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanEnd sp)

srcSpan :: SrcLoc -> SrcLoc -> SrcSpan
srcSpan b e | srcLine b > srcLine e || (srcLine b == srcLine e && srcColumn b > srcColumn e) = error $ "srcSpan " ++ show b ++ " > " ++ show e
srcSpan b e = SrcSpan {srcSpanFilename = srcFilename b,
                       srcSpanStartLine = srcLine b,
                       srcSpanStartColumn = srcColumn b,
                       srcSpanEndLine = srcLine e,
                       srcSpanEndColumn = srcColumn e}

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

srcLocSucc :: String -> SrcLoc -> SrcLoc
srcLocSucc text pos =
    case drop (srcLine pos - 1) (lines text) of
      [] -> pos {srcColumn = srcColumn pos + 1}
      (x : _) -> case drop (srcColumn pos - 1) x of
                   "" -> pos {srcLine = srcLine pos + 1, srcColumn = 1}
                   _ -> pos {srcColumn = srcColumn pos + 1}

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

----------------
-- UNIT TESTS --
----------------

tests :: Test
tests = TestList [test6, test7, test8]

test6 :: Test
test6 =
    TestList
    [ TestCase (assertEqual "lines' 0" [""] (lines' ""))
    , TestCase (assertEqual "lines' 1" ["abc"] (lines' "abc"))
    , TestCase (assertEqual "lines' 2" ["abc",""] (lines' "abc\n"))
    , TestCase (assertEqual "lines' 3" ["abc","","123"] (lines' "abc\n\n123"))
    , TestCase (assertEqual "lines' 4" ["abc","123",""] (lines' "abc\n123\n"))
    , TestCase (assertEqual "lines' 5" ["abc","","","123",""] (lines' "abc\n\n\n123\n")) ]

test7 :: Test
test7 =
    TestList
    [ TestCase (assertEqual "untabify 1" "abc" (untabify "abc"))
    , TestCase (assertEqual "untabify 2" "ab      c" (untabify "ab\tc"))
    , TestCase (assertEqual "untabify 3" "ab              c" (untabify "ab\t\tc"))
    , TestCase (assertEqual "untabify 4" "        abc" (untabify "\tabc"))
    , TestCase (assertEqual "untabify 5" "abcdefg h" (untabify "abcdefg\th"))
    , TestCase (assertEqual "untabify 6" "abcdefgh        i" (untabify "abcdefgh\ti")) ]

test8 :: Test
test8 =
    TestList
    [ TestCase (assertEqual "cutSrcLoc 1" ("abcd\n","efgh\nijkl\n") (cutSrcLoc (SrcLoc "<unknown>.hs" 2 1) "abcd\nefgh\nijkl\n"))
    , TestCase (assertEqual "cutSrcLoc 2" ("abcd\nef","gh\nijkl\n") (cutSrcLoc (SrcLoc "<unknown>.hs" 2 3) "abcd\nefgh\nijkl\n"))
    , TestCase (assertEqual "cutSrcLoc 3" ("abcd\nefgh","\nijkl\n") (cutSrcLoc (SrcLoc "<unknown>.hs" 2 5) "abcd\nefgh\nijkl\n"))
    , TestCase (assertEqual "cutSrcLoc 4" ("abcd\nefgh\n","ijkl") (cutSrcLoc (SrcLoc "<unknown>.hs" 3 1) "abcd\nefgh\nijkl"))
    , TestCase (assertEqual "cutSrcLoc 5" ("abcd\nefgh\nij","kl") (cutSrcLoc (SrcLoc "<unknown>.hs" 3 3) "abcd\nefgh\nijkl"))
    , TestCase (assertEqual "cutSrcLoc 6" ("abcd\nefgh\nijkl","") (cutSrcLoc (SrcLoc "<unknown>.hs" 3 5) "abcd\nefgh\nijkl")) ]
