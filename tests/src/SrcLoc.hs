{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module SrcLoc where

import Language.Haskell.Exts.SrcLoc (SrcLoc(..))
import Language.Haskell.Modules.SrcLoc (srcPairText)
import Prelude hiding (rem)
import Test.HUnit (assertEqual, Test(TestCase, TestList))

tests :: Test
tests = TestList [test1, test2, test3, test4, test5]

test1 :: Test
-- test1 = TestCase (assertEqual "srcPairTextTail1" "hi\tjkl\n" (snd (srcPairText (SrcLoc "<unknown>.hs" 1 10) (SrcLoc "<unknown>.hs" 2 2) "abc\tdef\nghi\tjkl\n")))
test1 = TestCase (assertEqual "srcPairTextTail1" "hi\tjkl\n" (snd (srcPairText (SrcLoc "<unknown>.hs" 1 6) (SrcLoc "<unknown>.hs" 2 2) "abc\tdef\nghi\tjkl\n")))
test2 :: Test
test2 = TestCase (assertEqual "srcPairTextTail2" "" (snd (srcPairText (SrcLoc "<unknown>.hs" 1 4) (SrcLoc "<unknown>.hs" 3 1) "abc\tdef\nghi\tjkl\n")))
test3 :: Test
test3 = TestCase (assertEqual "srcPairTextHead1" "abc\tdef\ng" (fst (srcPairText (SrcLoc "<unknown>.hs" 1 10) (SrcLoc "<unknown>.hs" 2 2) "abc\tdef\nghi\tjkl\n")))
test4 :: Test
test4 = TestCase (assertEqual "srcPairTextHead21" "abc\tdef\nghi\t" (fst (srcPairText (SrcLoc "<unknown>.hs" 1 5) (SrcLoc "<unknown>.hs" 2 5) "abc\tdef\nghi\tjkl\n")))
test5 :: Test
test5 = TestCase (assertEqual "srcPairTextTail3"
                              "{-# OPTIONS_GHC -fno-warn-orphans #-}\nmodule Debian.Repo.Orphans where\n\nimport Data.Text (Text)\nimport qualified Debian.Control.Text as T\n\nderiving instance Show (T.Field' Text)\nderiving instance Ord (T.Field' Text)\nderiving instance Show T.Paragraph\nderiving instance Ord T.Paragraph\n"
                              (snd
                               (srcPairText
                                 (SrcLoc "<unknown>.hs" 1 77)
                                 (SrcLoc "<unknown>.hs" 2 1)
                                 "\n{-# OPTIONS_GHC -fno-warn-orphans #-}\nmodule Debian.Repo.Orphans where\n\nimport Data.Text (Text)\nimport qualified Debian.Control.Text as T\n\nderiving instance Show (T.Field' Text)\nderiving instance Ord (T.Field' Text)\nderiving instance Show T.Paragraph\nderiving instance Ord T.Paragraph\n")))
