{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Util.SrcLoc
    ( HasSpanInfo(..)
    , srcSpan
    , srcLoc
    , endLoc
    , textEndLoc
    , increaseSrcLoc
    , textSpan
    , srcPairTextHead
    , srcPairTextTail
    , srcPairTextPair
    , makeTree
    , tests
    ) where

import Data.Default (def, Default)
import Data.List (groupBy, partition, sort)
import Data.Set (Set, toList)
import Data.Tree (Tree(Node), unfoldTree)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(..), ExportSpec(..), ExportSpecList(..), ImportDecl(ImportDecl), ModuleHead(..), ModuleName(..), ModulePragma(..), WarningText(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Prelude hiding (rem)
import Test.HUnit (assertEqual, Test(TestCase, TestList))

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

class HasSpanInfo a where
    spanInfo :: a -> SrcSpanInfo

instance HasSpanInfo SrcSpan where
    spanInfo x = SrcSpanInfo x []

instance HasSpanInfo a => HasSpanInfo (Tree a) where
    spanInfo (Node x _) = spanInfo x

instance HasSpanInfo ModuleHead where
    spanInfo (A.ModuleHead x _ _ _) = x

instance HasSpanInfo ModuleName where
    spanInfo (A.ModuleName x _) = x

instance HasSpanInfo ModulePragma where
    spanInfo (A.LanguagePragma x _) = x
    spanInfo (A.OptionsPragma x _ _) = x
    spanInfo (A.AnnModulePragma x _) = x

instance HasSpanInfo WarningText where
    spanInfo (A.WarnText x _) = x
    spanInfo (A.DeprText x _) = x

instance HasSpanInfo ExportSpecList where
    spanInfo (A.ExportSpecList x _) = x

instance HasSpanInfo ExportSpec where
    spanInfo (A.EVar x _) = x
    spanInfo (A.EAbs x _) = x
    spanInfo (A.EThingAll x _) = x
    spanInfo (A.EThingWith x _ _) = x
    spanInfo (A.EModuleContents x _) = x

instance HasSpanInfo ImportDecl where
    spanInfo (A.ImportDecl x _ _ _ _ _ _) = x

instance HasSpanInfo Decl where
    spanInfo (A.TypeDecl l _ _) = l
    spanInfo (A.TypeFamDecl l _ _) = l
    spanInfo (A.DataDecl l _ _ _ _ _) = l
    spanInfo (A.GDataDecl l _ _ _ _ _ _) = l
    spanInfo (A.DataFamDecl l _ _ _) = l
    spanInfo (A.TypeInsDecl l _ _) = l
    spanInfo (A.DataInsDecl l _ _ _ _) = l
    spanInfo (A.GDataInsDecl l _ _ _ _ _) = l
    spanInfo (A.ClassDecl l _ _ _ _) = l
    spanInfo (A.InstDecl l  _ _ _) = l
    spanInfo (A.DerivDecl l _ _) = l
    spanInfo (A.InfixDecl l _ _ _) = l
    spanInfo (A.DefaultDecl l _) = l
    spanInfo (A.SpliceDecl l _) = l
    spanInfo (A.TypeSig l _ _) = l
    spanInfo (A.FunBind l _) = l
    spanInfo (A.PatBind l _ _ _ _) = l
    spanInfo (A.ForImp l _ _ _ _ _) = l
    spanInfo (A.ForExp l _ _ _ _) = l
    spanInfo (A.RulePragmaDecl l _) = l
    spanInfo (A.DeprPragmaDecl l _) = l
    spanInfo (A.WarnPragmaDecl l _) = l
    spanInfo (A.InlineSig l _ _ _) = l
    spanInfo (A.InlineConlikeSig l _ _) = l
    spanInfo (A.SpecSig l _ _) = l
    spanInfo (A.SpecInlineSig l _ _ _ _) = l
    spanInfo (A.InstSig l _ _) = l
    spanInfo (A.AnnPragma l _) = l

instance HasSpanInfo SrcSpanInfo where
    spanInfo = id

{-
data SrcSpanInfo
  = SrcSpanInfo { srcInfoSpan :: SrcSpan
                , srcInfoPoints :: [SrcSpan] }

data SrcSpan
  = SrcSpan {srcSpanFilename :: String,
             srcSpanStartLine :: Int,
             srcSpanStartColumn :: Int,
             srcSpanEndLine :: Int,
             srcSpanEndColumn :: Int}

data SrcLoc
  = SrcLoc {srcFilename :: String, srcLine :: Int, srcColumn :: Int}
-}

srcSpan :: HasSpanInfo x => x -> SrcSpan
srcSpan = srcInfoSpan . spanInfo

srcLoc :: HasSpanInfo x => x -> SrcLoc
srcLoc x = let (SrcSpan f b e _ _) = srcSpan x in SrcLoc f b e

endLoc :: HasSpanInfo x => x -> SrcLoc
endLoc x = let (SrcSpan f _ _ b e) = srcSpan x in SrcLoc f b e

textEndLoc :: String -> SrcLoc
textEndLoc text =
    def {srcLine = length ls, srcColumn = length (last ls) + 1}
    where ls = lines' text

-- | Update a SrcLoc to move it from l past the string argument.
increaseSrcLoc :: String -> SrcLoc -> SrcLoc
increaseSrcLoc "" l = l
increaseSrcLoc ('\n' : s) (SrcLoc f y _) = increaseSrcLoc s (SrcLoc f (y + 1) 1)
increaseSrcLoc (_ : s) (SrcLoc f y x) = increaseSrcLoc s (SrcLoc f y (x + 1))

tests :: Test
tests = TestList [test1, test2, test3, test4, test5]

test1 :: Test
test1 = TestCase (assertEqual "srcPairTextTail1" "hi\tjkl\n" (srcPairTextTail (SrcLoc "<unknown>.hs" 1 10) (SrcLoc "<unknown>.hs" 2 2) "abc\tdef\nghi\tjkl\n"))
test2 :: Test
test2 = TestCase (assertEqual "srcPairTextTail2" "kl\n" (srcPairTextTail (SrcLoc "<unknown>.hs" 1 10) (SrcLoc "<unknown>.hs" 2 9) "abc\tdef\nghi\tjkl\n"))
test3 :: Test
test3 = TestCase (assertEqual "srcPairTextHead1" "abc\tdef\ng" (srcPairTextHead (SrcLoc "<unknown>.hs" 1 10) (SrcLoc "<unknown>.hs" 2 2) "abc\tdef\nghi\tjkl\n"))
test4 :: Test
test4 = TestCase (assertEqual "srcPairTextHead21" "abc\tdef\nghi\tj" (srcPairTextHead (SrcLoc "<unknown>.hs" 1 10) (SrcLoc "<unknown>.hs" 2 9) "abc\tdef\nghi\tjkl\n"))
test5 :: Test
test5 = TestCase (assertEqual "srcPairTextTail3"
                              "{-# OPTIONS_GHC -fno-warn-orphans #-}\nmodule Debian.Repo.Orphans where\n\nimport Data.Text (Text)\nimport qualified Debian.Control.Text as T\n\nderiving instance Show (T.Field' Text)\nderiving instance Ord (T.Field' Text)\nderiving instance Show T.Paragraph\nderiving instance Ord T.Paragraph\n"
                              (srcPairTextTail
                                 (SrcLoc "<unknown>.hs" 1 77)
                                 (SrcLoc "<unknown>.hs" 2 1)
                                 "\n{-# OPTIONS_GHC -fno-warn-orphans #-}\nmodule Debian.Repo.Orphans where\n\nimport Data.Text (Text)\nimport qualified Debian.Control.Text as T\n\nderiving instance Show (T.Field' Text)\nderiving instance Ord (T.Field' Text)\nderiving instance Show T.Paragraph\nderiving instance Ord T.Paragraph\n"))

textSpan :: String -> SrcSpanInfo
textSpan s = let end = textEndLoc s in
             SrcSpanInfo (def {srcSpanStartLine = 1, srcSpanStartColumn = 1, srcSpanEndLine = srcLine end, srcSpanEndColumn = srcColumn end}) []

-- | Return the beginning portion of s which the span b thru e covers,
-- assuming that the beginning of s is at position b - that is, that
-- the prefix of s from (1,1) to b has already been removed.
srcPairTextHead :: SrcLoc -> SrcLoc -> String -> String
srcPairTextHead b0 e0 s0 =
    f b0 e0 [] s0
    where
      f b e r s =
          if srcLine b < srcLine e
          then case span (/= '\n') s of
                 (r', '\n' : s') ->
                     f (b {srcLine = srcLine b + 1, srcColumn = 1}) e ("\n" : r' : r) s'
                 (_, "") ->
                     -- This should not happen, but if the last line
                     -- lacks a newline terminator, haskell-src-exts
                     -- will set the end location as if the terminator
                     -- was present.
                     case s of
                       "" -> concat (reverse r)
                       ('\t' : s') -> f (b {srcColumn = ((srcColumn b + 7) `div` 8) * 8}) e (['\t'] : r) s'
                       (c : s') -> f (b {srcColumn = srcColumn b + 1}) e ([c] : r) s'
                 _ -> error $ "srcPairTextHead: " ++ show (b, e, s)
          else if srcColumn b < srcColumn e
               then case s of
                      [] -> error $ "srcPairTextHead: " ++ show (b0, e0, s0)
                      ('\t' : s') -> f (b {srcColumn = ((srcColumn b + 7) `div` 8) * 8}) e (['\t'] : r) s'
                      (c : s') -> f (b {srcColumn = srcColumn b + 1}) e ([c] : r) s'
               else concat (reverse r)

-- | Like srcPairTextHead, but returns the tail of s instead of the head.
srcPairTextTail :: SrcLoc -> SrcLoc -> String -> String
srcPairTextTail b e s =
    if srcLine b < srcLine e
    then case dropWhile (/= '\n') s of
           ('\n' : s') -> srcPairTextTail (b {srcLine = srcLine b + 1, srcColumn = 1}) e s'
           -- This should not happen, but if the last line lacks a
           -- newline terminator, haskell-src-exts will set the end
           -- location as if the terminator was present.
           [] -> case s of
                   [] -> ""
                   ('\t' : s') -> srcPairTextTail (b {srcColumn = ((srcColumn b + 7) `div` 8) * 8}) e s'
                   (_ : s') -> srcPairTextTail (b {srcColumn = srcColumn b + 1}) e s'
           _ -> error $ "srcPairTextTail: b=" ++ show b ++ ", e=" ++ show e ++ ", s = " ++ show s
    else if srcColumn b < srcColumn e
         then case s of
                [] -> error $ "srcPairTextTail: b=" ++ show b ++ ", e=" ++ show e ++ ", s = " ++ show s
                ('\t' : s') -> srcPairTextTail (b {srcColumn = ((srcColumn b + 7) `div` 8) * 8}) e s'
                (_ : s') -> srcPairTextTail (b {srcColumn = srcColumn b + 1}) e s'
         else s

-- | Shirley there's a more efficient way to do this?
srcPairTextPair :: SrcLoc -> SrcLoc -> String -> (String, String)
srcPairTextPair b e s = (srcPairTextHead b e s, srcPairTextTail b e s)

instance Default SrcLoc where
    def = SrcLoc "<unknown>.hs" 1 1

instance Default SrcSpanInfo where
    def = SrcSpanInfo {srcInfoSpan = def, srcInfoPoints = def}

instance Default SrcSpan where
    def = SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 1, srcSpanEndLine = 1, srcSpanStartColumn = 1, srcSpanEndColumn = 1}

-- | Build a tree of SrcSpanInfo 

makeTree :: (HasSpanInfo a, Show a, Eq a, Ord a) => Set a -> Tree a
makeTree s =
    case findRoots (toList s) of
      [] -> error "No roots"
      [root] -> unfoldTree f root
      roots -> error $ "Multiple roots: " ++ show roots
    where
      f x = (x, findChildren (toList s) x)

      -- The roots are the nodes that are not covered by any other node.
      findRoots :: (HasSpanInfo a, Eq a, Ord a) => [a] -> [a]
      findRoots [] = []
      findRoots (x : xs) =
          let (_children, other) = partition (\ y -> x `covers` y) xs
              (ancestors, cousins) = partition (\ y -> x `coveredBy` y) other in
          case ancestors of
            -- If there are no ancestors, x is a root, and there may be other roots among the cousins
            [] -> x : findRoots cousins
            -- If there are ancestors, there must be a root among them, and there still may be roots among the cousins.
            _ -> findRoots (ancestors ++ cousins)

      findChildren :: (HasSpanInfo a, Eq a, Ord a, Show a) => [a] -> a -> [a]
      findChildren u x = findRoots children where children = sort (filter (\ y -> x `covers` y && x /= y) u)

-- True if a covers b
covers :: (HasSpanInfo a, HasSpanInfo b) => a -> b -> Bool
covers a b = srcLoc a <= srcLoc b && endLoc b <= endLoc a

-- True if a is covered by b
coveredBy :: (HasSpanInfo a, HasSpanInfo b) => a -> b -> Bool
coveredBy = flip covers

{-
test3 = TestCase (assertEqual "covers1" True (covers (mkspan (29, 1) (29, 8)) (mkspan (29, 3) (29, 7))))
test4 = TestCase (assertEqual "covers2" False (covers (mkspan (29, 1) (29, 8)) (mkspan (29, 3) (29, 10))))
test5 = TestCase (assertEqual "roots1"
                                  [sp 5 10, sp 11 18]
                                  (findRoots [sp 5 10,
                                              sp 11 18,
                                              sp 6 7,
                                              sp 8 9,
                                              sp 12 15]))
-}
