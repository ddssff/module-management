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
    , tests
    ) where

import Data.Default (def, Default)
import Data.List (intercalate)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), srcSpanEnd, srcSpanStart)
import Language.Haskell.Exts.Syntax (Decl(..), ImportDecl(..), Match(..), Module(..))
import Language.Haskell.Imports.Common (Display(..), HasSrcLoc(..), HasSrcSpan(..), lines', untabify)
import Test.HUnit (assertEqual, Test(TestCase, TestList))

data SrcUnion a
    = Head' a Module
    | Comment' a Comment
    | ImportDecl' a ImportDecl
    | Decl' a Decl
    -- | Space' String SrcSpan
    | Space' a String SrcLoc
    | Other' a String SrcLoc
    deriving (Eq, Show)

instance Display (SrcUnion SrcSpan) where
    display (Comment' sp _) = "Comment' " ++ display sp
    display (Other' sp _ _) = "Other' " ++ display sp
    display (Space' sp _ _) = "Space' " ++ display sp
    display (ImportDecl' sp _) = "ImportDecl' " ++ display sp
    display (Decl' sp _) = "Decl' " ++ display sp
    display (Head' sp _) = "Head' " ++ display sp

instance HasSrcSpan (SrcUnion SrcSpan) where
    srcSpan' (Comment' sp _) = sp
    srcSpan' (Other' sp _ _) = sp
    srcSpan' (Space' sp _ _) = sp
    srcSpan' (ImportDecl' sp _) = sp
    srcSpan' (Decl' sp _) = sp
    srcSpan' (Head' sp _) = sp

instance Display (SrcUnion ()) where
    display (Comment' () c) = display c
    display (Other' () s l) = "Other' " ++ display l ++ " " ++ show s
    display (Space' () s l) = "Space' " ++ display l ++ " " ++ show s
    display (ImportDecl' () x) = display x
    display (Decl' () x) = display x
    display (Head' () x) = display x

getA :: SrcUnion a -> a
getA (Head' x _) = x
getA (Comment' x _) = x
getA (ImportDecl' x _) = x
getA (Decl' x _) = x
getA (Space' x _ _) = x
getA (Other' x _ _) = x

{-
mapA :: (a -> b) -> SrcUnion a -> SrcUnion b
mapA f (Head' x y) = Head' (f x) y
mapA f (Comment' x y) = Comment' (f x) y
mapA f (ImportDecl' x y) = ImportDecl' (f x) y
mapA f (Decl' x y) = Decl' (f x) y
mapA f (Space' x y z) = Space' (f x) y z
mapA f (Other' x y z) = Other' (f x) y z
-}

instance HasSrcLoc (SrcUnion a) where
    srcLoc (Head' _ x) = srcLoc x
    srcLoc (Comment' _ x) = srcLoc x
    srcLoc (ImportDecl' _ x) = srcLoc x
    srcLoc (Decl' _ x) = srcLoc x
    srcLoc (Space' _ _ l) = l
    srcLoc (Other' _ _ l) = l

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

instance HasEndLoc (SrcUnion SrcSpan) where
    endLoc = srcSpanEnd' . getA

instance HasEndLoc SrcSpan where
    endLoc (SrcSpan f _ _ b e) = SrcLoc f b e

instance HasEndLoc Comment where
    endLoc (Comment _ sp _) = srcSpanEnd' sp

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
