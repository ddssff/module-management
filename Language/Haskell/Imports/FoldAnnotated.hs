{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Imports.FoldAnnotated
    ( foldModule
    , tests
    ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy.Char8 as B (readFile)
import Data.Char (isSpace)
import Data.Default (def, Default)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.List (partition, sortBy)
import Language.Haskell.Exts.Annotated (defaultParseMode, parseFileWithComments, ParseResult(..))
import Language.Haskell.Exts.Annotated.Syntax (ImportDecl(..), Module(..), ModulePragma(..), Decl(..), ModuleHead(..), ModuleName(..), WarningText(..), ExportSpecList(..))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Imports.Common (Display(..), untabify, lines', withCurrentDirectory)
import Language.Haskell.Imports.SrcLoc (HasSrcSpan(..), HasEndLoc(..), HasSrcLoc(..), srcPairText, srcSpanEnd', srcSpanText, textEndLoc)
import Test.HUnit (assertEqual, Test(TestLabel, TestCase, TestList))

class PutSrcSpan a where
    putSrcSpan :: SrcSpan -> a -> a

instance HasSrcLoc SrcSpanInfo where
    srcLoc = srcLoc . srcInfoSpan

instance HasSrcLoc (ImportDecl SrcSpanInfo) where
    srcLoc = srcLoc . importAnn

instance HasSrcLoc (Decl SrcSpanInfo) where
    srcLoc = srcLoc . srcSpan

instance HasSrcLoc (Module SrcSpanInfo) where
    srcLoc (Module l _ _ _ _) = srcLoc l
    srcLoc (XmlPage l _ _ _ _ _ _) = srcLoc l
    srcLoc (XmlHybrid l _ _ _ _ _ _ _ _) = srcLoc l

instance HasSrcLoc (ModuleHead SrcSpanInfo) where
    srcLoc (ModuleHead x _ _ _) = srcLoc x

instance HasSrcSpan (ModuleHead SrcSpanInfo) where
    srcSpan (ModuleHead x _ _ _) = srcSpan x

instance HasSrcSpan (ModuleName SrcSpanInfo) where
    srcSpan (ModuleName x _) = srcSpan x

instance HasSrcSpan (WarningText SrcSpanInfo) where
    srcSpan (WarnText x _) = srcSpan x

instance HasSrcSpan (ExportSpecList SrcSpanInfo) where
    srcSpan (ExportSpecList x _) = srcSpan x

instance HasSrcSpan SrcSpanInfo where
    srcSpan = srcInfoSpan

instance HasEndLoc (ModuleHead SrcSpanInfo) where
    endLoc = endLoc . srcSpan

instance PutSrcSpan SrcSpanInfo where
    putSrcSpan sp x = x {srcInfoSpan = sp} -- what should we do about srcInfoPoints?

instance PutSrcSpan (ModuleHead SrcSpanInfo) where
    putSrcSpan sp (ModuleHead x a b c) = ModuleHead (putSrcSpan sp x) a b c

instance PutSrcSpan (ModuleName SrcSpanInfo) where
    putSrcSpan sp (ModuleName x a) = ModuleName (putSrcSpan sp x) a

instance PutSrcSpan (WarningText SrcSpanInfo) where
    putSrcSpan sp (WarnText x a) = WarnText (putSrcSpan sp x) a

instance PutSrcSpan (ExportSpecList SrcSpanInfo) where
    putSrcSpan sp (ExportSpecList x a) = ExportSpecList (putSrcSpan sp x) a

instance PutSrcSpan (ModulePragma SrcSpanInfo) where
    putSrcSpan sp (LanguagePragma l a) = LanguagePragma (putSrcSpan sp l) a
    putSrcSpan sp (OptionsPragma l a b) = OptionsPragma (putSrcSpan sp l) a b
    putSrcSpan sp (AnnModulePragma l a) = AnnModulePragma (putSrcSpan sp l) a

instance PutSrcSpan Comment where
    putSrcSpan sp (Comment a _ b) = Comment a sp b

instance PutSrcSpan (ImportDecl SrcSpanInfo) where
    putSrcSpan sp (ImportDecl x a b c d e f) = ImportDecl (putSrcSpan sp x) a b c d e f

instance HasEndLoc (ImportDecl SrcSpanInfo) where
    endLoc = endLoc . srcSpan

instance HasEndLoc (Decl SrcSpanInfo) where
    endLoc = endLoc . srcSpan

instance HasSrcSpan Comment where
    srcSpan (Comment _ sp _) = sp

instance HasSrcSpan (ImportDecl SrcSpanInfo) where
    srcSpan = srcSpan . importAnn

instance HasSrcSpan (ModulePragma SrcSpanInfo) where
    srcSpan (LanguagePragma l _) = srcSpan l
    srcSpan (OptionsPragma l _ _) = srcSpan l
    srcSpan (AnnModulePragma l _) = srcSpan l

instance HasSrcSpan (Decl SrcSpanInfo) where
    srcSpan (TypeDecl l _ _) = srcSpan l
    srcSpan (TypeFamDecl l _ _) = srcSpan l
    srcSpan (DataDecl l _ _ _ _ _) = srcSpan l
    srcSpan (GDataDecl l _ _ _ _ _ _) = srcSpan l
    srcSpan (DataFamDecl l _ _ _) = srcSpan l
    srcSpan (TypeInsDecl l _ _) = srcSpan l
    srcSpan (DataInsDecl l _ _ _ _) = srcSpan l
    srcSpan (GDataInsDecl l _ _ _ _ _) = srcSpan l
    srcSpan (ClassDecl l _ _ _ _) = srcSpan l
    srcSpan (InstDecl l  _ _ _) = srcSpan l
    srcSpan (DerivDecl l _ _) = srcSpan l
    srcSpan (InfixDecl l _ _ _) = srcSpan l
    srcSpan (DefaultDecl l _) = srcSpan l
    srcSpan (SpliceDecl l _) = srcSpan l
    srcSpan (TypeSig l _ _) = srcSpan l
    srcSpan (FunBind l _) = srcSpan l
    srcSpan (PatBind l _ _ _ _) = srcSpan l
    srcSpan (ForImp l _ _ _ _ _) = srcSpan l
    srcSpan (ForExp l _ _ _ _) = srcSpan l
    srcSpan (RulePragmaDecl l _) = srcSpan l
    srcSpan (DeprPragmaDecl l _) = srcSpan l
    srcSpan (WarnPragmaDecl l _) = srcSpan l
    srcSpan (InlineSig l _ _ _) = srcSpan l
    srcSpan (InlineConlikeSig l _ _) = srcSpan l
    srcSpan (SpecSig l _ _) = srcSpan l
    srcSpan (SpecInlineSig l _ _ _ _) = srcSpan l
    srcSpan (InstSig l _ _) = srcSpan l
    srcSpan (AnnPragma l _) = srcSpan l

instance PutSrcSpan (Decl SrcSpanInfo) where
    putSrcSpan sp (TypeDecl l a b) = TypeDecl (putSrcSpan sp l) a b
    putSrcSpan sp (TypeFamDecl l a b) = TypeFamDecl (putSrcSpan sp l) a b
    putSrcSpan sp (DataDecl l a b c d e) = DataDecl (putSrcSpan sp l) a b c d e
    putSrcSpan sp (GDataDecl l a b c d e f) = GDataDecl (putSrcSpan sp l) a b c d e f
    putSrcSpan sp (DataFamDecl l a b c) = DataFamDecl (putSrcSpan sp l) a b c
    putSrcSpan sp (TypeInsDecl l a b) = TypeInsDecl (putSrcSpan sp l) a b
    putSrcSpan sp (DataInsDecl l a b c d) = DataInsDecl (putSrcSpan sp l) a b c d
    putSrcSpan sp (GDataInsDecl l a b c d e) = GDataInsDecl (putSrcSpan sp l) a b c d e
    putSrcSpan sp (ClassDecl l a b c d) = ClassDecl (putSrcSpan sp l) a b c d
    putSrcSpan sp (InstDecl l a b c) = InstDecl (putSrcSpan sp l) a b c
    putSrcSpan sp (DerivDecl l a b) = DerivDecl (putSrcSpan sp l) a b
    putSrcSpan sp (InfixDecl l a b c) = InfixDecl (putSrcSpan sp l) a b c
    putSrcSpan sp (DefaultDecl l a) = DefaultDecl (putSrcSpan sp l) a
    putSrcSpan sp (SpliceDecl l a) = SpliceDecl (putSrcSpan sp l) a
    putSrcSpan sp (TypeSig l a b) = TypeSig (putSrcSpan sp l) a b
    putSrcSpan sp (FunBind l a) = FunBind (putSrcSpan sp l) a
    putSrcSpan sp (PatBind l a b c d) = PatBind (putSrcSpan sp l) a b c d
    putSrcSpan sp (ForImp l a b c d e) = ForImp (putSrcSpan sp l) a b c d e
    putSrcSpan sp (ForExp l a b c d) = ForExp (putSrcSpan sp l) a b c d
    putSrcSpan sp (RulePragmaDecl l a) = RulePragmaDecl (putSrcSpan sp l) a
    putSrcSpan sp (DeprPragmaDecl l a) = DeprPragmaDecl (putSrcSpan sp l) a
    putSrcSpan sp (WarnPragmaDecl l a) = WarnPragmaDecl (putSrcSpan sp l) a
    putSrcSpan sp (InlineSig l a b c) = InlineSig (putSrcSpan sp l) a b c
    putSrcSpan sp (InlineConlikeSig l a b) = InlineConlikeSig (putSrcSpan sp l) a b
    putSrcSpan sp (SpecSig l a b) = SpecSig (putSrcSpan sp l) a b
    putSrcSpan sp (SpecInlineSig l a b c d) = SpecInlineSig (putSrcSpan sp l) a b c d
    putSrcSpan sp (InstSig l a b) = InstSig (putSrcSpan sp l) a b
    putSrcSpan sp (AnnPragma l a) = AnnPragma (putSrcSpan sp l) a

{-
data Module l
  = Module l
           (Maybe (ModuleHead l))
           [ModulePragma l]
           [ImportDecl l]
           [Decl l]

data ModuleHead l
  = ModuleHead l
               (ModuleName l)
               (Maybe (WarningText l))
               (Maybe (ExportSpecList l))

data Comment = Comment Bool SrcSpan String
  	-- Defined in `Language.Haskell.Exts.Comments'

data ImportDecl l
  = ImportDecl {importAnn :: l,
                importModule :: ModuleName l,
                importQualified :: Bool,
                importSrc :: Bool,
                importPkg :: Maybe String,
                importAs :: Maybe (ModuleName l),
                importSpecs :: Maybe (ImportSpecList l)}

data ModulePragma l
  = LanguagePragma l [Name l]
  | OptionsPragma l (Maybe Tool) String
  | AnnModulePragma l (Annotation l)

data SrcSpanInfo
  = SrcSpanInfo {srcInfoSpan :: SrcSpan, srcInfoPoints :: [SrcSpan]}
  	-- Defined in `Language.Haskell.Exts.SrcLoc'
-}

data SrcUnion
    = Name' (ModuleName SrcSpanInfo)
    | Warning' (WarningText SrcSpanInfo)
    | Exports' (ExportSpecList SrcSpanInfo)
    | Comment' Comment
    | Pragma' (ModulePragma SrcSpanInfo)
    | ImportDecl' (ImportDecl SrcSpanInfo)
    | Decl' (Decl SrcSpanInfo)
    | Space' String SrcSpan
    | Other' String SrcSpan
    deriving (Eq, Show)

instance Display SrcUnion where
    display (Comment' c) = "Comment' " ++ display (srcSpan c)
    display (Other' _s l) = "Other' " ++ display l
    display (Space' _s l) = "Space' " ++ display l
    display (ImportDecl' x) = "ImportDecl' " ++ display (srcSpan x)
    display (Decl' x) = "Decl' " ++ display (srcSpan x)
    display (Name' x) = "Name' " ++ display (srcSpan x)
    display (Warning' x) = "Warning' " ++ display (srcSpan x)
    display (Exports' x) = "Exports' " ++ display (srcSpan x)
    display (Pragma' x) = "Pragma' " ++ display (srcSpan x)

{-
instance Display (ImportDecl SrcSpanInfo) where
    display _ = "ImportDecl"

instance Display (Decl SrcSpanInfo) where
    display _ = "Decl"

instance Display (ModuleHead SrcSpanInfo) where
    display _ = "ModuleHead"

instance Display (ModulePragma SrcSpanInfo) where
    display _ = "ModulePragma"
-}

instance HasSrcSpan SrcUnion where
    srcSpan (Name' x) = srcSpan x
    srcSpan (Warning' x) = srcSpan x
    srcSpan (Exports' x) = srcSpan x
    srcSpan (Pragma' x) = srcSpan x
    srcSpan (Comment' x) = srcSpan x
    srcSpan (ImportDecl' x) = srcSpan x
    srcSpan (Decl' x) = srcSpan x
    srcSpan (Space' _ sp) = sp
    srcSpan (Other' _ sp) = sp

instance PutSrcSpan SrcUnion where
    putSrcSpan sp (Name' x) = Name' (putSrcSpan sp x)
    putSrcSpan sp (Warning' x) = Warning' (putSrcSpan sp x)
    putSrcSpan sp (Exports' x) = Exports' (putSrcSpan sp x)
    putSrcSpan sp (Pragma' x) = Pragma' (putSrcSpan sp x)
    putSrcSpan sp (Comment' x) = Comment' (putSrcSpan sp x)
    putSrcSpan sp (ImportDecl' x) = ImportDecl' (putSrcSpan sp x)
    putSrcSpan sp (Decl' x) = Decl' (putSrcSpan sp x)
    putSrcSpan sp (Space' s _) = Space' s sp
    putSrcSpan sp (Other' s _) = Other' s sp

instance HasSrcLoc SrcUnion where
    srcLoc = srcLoc . srcSpan

instance HasEndLoc SrcUnion where
    endLoc = endLoc . srcSpan

mapA :: (SrcSpan -> SrcSpan) -> SrcUnion -> SrcUnion
mapA f u = putSrcSpan (f (srcSpan u)) u

-- Note that comments will overlap with the other elements
foldModule :: forall r.
              (ModulePragma SrcSpanInfo -> String -> String -> r -> r)
           -> (ModuleName SrcSpanInfo -> String -> String -> r -> r)
           -> (WarningText SrcSpanInfo -> String -> String -> r -> r)
           -> (ExportSpecList SrcSpanInfo -> String -> String -> r -> r)
           -> (ImportDecl SrcSpanInfo -> String -> String -> r -> r)
           -> (Decl SrcSpanInfo -> String -> String -> r -> r)
           -> (String -> r -> r)
           -> Module SrcSpanInfo -> [Comment] -> String -> r -> r
foldModule pragmaf namef warningf exportsf importf declf spacef m comments text0 r0 =
    doItems Nothing (moduleItemsFinal text m comments) r0
    where
      doItems :: Maybe (String, SrcSpan) -> [SrcUnion] -> r -> r
      doItems Nothing (Space' s sp : xs) r = doItems (Just (s, sp)) xs r
      doItems pre (x : y : xs) r = let r' = doItem pre x (srcLoc x) (srcLoc y) r in doItems Nothing (y : xs) r'
      doItems pre [x] r = doItem pre x (srcLoc x) (textEndLoc text) r -- x is not a Space' here
      doItems (Just (s, sp)) [] r = let x = Space' s sp in doItem Nothing x (srcLoc sp) (textEndLoc text) r
      doItems Nothing [] r = r

      doItem :: Maybe (String, SrcSpan) -> SrcUnion -> SrcLoc -> SrcLoc -> r -> r
      doItem pre (Pragma' x) b e r = pragmaf x (maybe "" fst pre) (srcPairText b e text) r
      doItem pre (Name' x) b e r = namef x (maybe "" fst pre) (srcPairText b e text) r
      doItem pre (Warning' x) b e r = warningf x (maybe "" fst pre) (srcPairText b e text) r
      doItem pre (Exports' x) b e r = exportsf x (maybe "" fst pre) (srcPairText b e text) r
      doItem pre (ImportDecl' x) b e r = importf x (maybe "" fst pre) (srcPairText b e text) r
      doItem pre (Decl' x) b e r = declf x (maybe "" fst pre) (srcPairText b e text) r
      doItem Nothing (Space' _ _) b e r = spacef (srcPairText b e text) {-(srcSpan b e)-} r
      doItem _ (Space' _ sp) _ _ _ = error $ "Unexpected pair of Space' constructors at " ++ display sp
      -- These should have been converted to Space
      doItem _ (Comment' _) _ _ _ = error "Unexpected: Comment'"
      doItem _ (Other' _ _) _ _ _ = error "Unexpected: Other'"
      text = untabify text0

-- | If two elements have the same end position, one will be space and
-- one will be code.  Move the end of the code element to just before
-- the beginning of the space element.  Finally, remove any space
-- items that start after their successor, these are embedded comments
-- which we can't do anything with.
moduleItemsFinal :: String -> Module SrcSpanInfo -> [Comment] -> [SrcUnion]
moduleItemsFinal text m comments =
    sortBy (compare `on` srcSpan) $ concatMap (adjust . sortBy (compare `on` srcLoc)) groups
    where
      -- Remove embedded spans from the list - those that begin after
      -- and end before the union of all the spans.
      groups = map filterEmbedded (moduleItemGroups text m comments)
      -- Adjust the remaining elements so they don't overlap.
      adjust xs =
          case partition ((== sp) . srcSpan) xs of
            ([x], ys) ->
                case ys of
                  [y] ->
                      [mapA (const (srcSpan (srcLoc x, srcLoc y))) x,
                       mapA (const (srcSpan (srcLoc y, endLoc x))) y]
                  [] -> [x]
            ([], _) -> error "adjust: No covering element"
            _ -> error "adjust: multiple spanning elements"
          where
            sp = foldr1 mergeSrcSpan (map srcSpan xs)

-- | Zip the decls and space together sorted by end position.  Collect overlapping items into groups.
-- To collect overlaps, we first sort by endLoc and reverse the list.  As we scan this list, items whose start location precedes 
moduleItemGroups :: String -> Module SrcSpanInfo -> [Comment] -> [[SrcUnion]]
moduleItemGroups text m comments =
    reverse $ groups
    where
      -- Given the items sorted in order of descending end location,
      -- we start a new group whenever we see an end location equal to
      -- the smallest start location yet seen
      groups =
          let (x : xs) = descendingEnds in
          loop (srcLoc x) (endLoc x) [[x]] xs
          where
            loop _ _ rss [] = rss
            loop b e (r : rs) (y : ys) =
                if endLoc y == e || srcLoc y >= b
                then loop (min b (srcLoc y)) (max e (endLoc y)) ((y : r) : rs) ys
                else loop (srcLoc y) (endLoc y) ([y] : (r : rs)) ys
      descendingEnds = reverse $ sortBy (compare `on` endLoc) $ decls ++ space
      decls = moduleDecls text m
      space = moduleSpace text comments

filterEmbedded :: [SrcUnion] -> [SrcUnion]
filterEmbedded xs =
    filter (not . embedded) xs
    where
      embedded x = srcLoc sp < srcLoc x && endLoc x < endLoc sp
      sp = foldr1 mergeSrcSpan (map srcSpan xs)

-- | Wrap the source code elements in SrcUnion constructors, sort, and elements to cover any spaces
moduleDecls :: String -> Module SrcSpanInfo -> [SrcUnion]
moduleDecls text (Module _ mh ps imps decls) =
    case insertSpaceItems text
             (sortBy
                (compare `on` srcLoc)
                (map Pragma' ps ++
                 maybe [] (\ h@(ModuleHead _ n w e) ->
                               [Name' n] ++
                               (maybe [] (\ x -> [Warning' x]) w) ++
                               (maybe [] (\ x -> [Exports' x]) e)) mh ++
                 map ImportDecl' imps ++
                 map Decl' decls)) of
      Other' {} : xs -> xs
      xs -> xs

-- | Same for comments, then group runs of Comment or Space, these are guaranteed to be adjacent
moduleSpace :: String -> [Comment] -> [SrcUnion]
moduleSpace text comments =
    groupSpace text $ insertSpaceItems text $ sortBy (compare `on` srcLoc) (map Comment' comments)

insertSpaceItems :: String -> [SrcUnion] -> [SrcUnion]
insertSpaceItems text items =
    loop def items
    where
      loop :: SrcLoc -> [SrcUnion] -> [SrcUnion]
      loop loc [] = gap loc (textEndLoc text)
      loop loc (x : xs) | loc < srcLoc x = gap loc (srcLoc x) ++ loop (srcLoc x) (x : xs)
      loop loc (x : xs) =
          let end = next xs in
          case x of
            Comment' c@(Comment _ _ _) ->
                Comment' c : loop (endLoc c) xs
            _ -> mapA (const (srcSpan (loc, end))) x : loop end xs
      gap :: SrcLoc -> SrcLoc -> [SrcUnion]
      gap b e =
          let s = srcPairText b e text
              sp = srcSpan (b, e) in
          case span isSpace s of
            ("", "") -> []
            ("", _) -> [Other' s sp]
            (_, "") -> [Space' s sp]
            (s', t) ->
                let b' = appendLoc s' b in
                [Space' s' (srcSpan (b, b')),
                 Other' t (srcSpan (b', (srcSpanEnd' sp)))]

      next :: [SrcUnion] -> SrcLoc
      next [] = textEndLoc text
      next (x : _) = srcLoc x

-- | Given a list of Comment, Space and Other elements, discard the
-- Other elements, group adjoining elements, and then turn each group
-- of adjacent elements into a single Space element.
groupSpace :: String -> [SrcUnion] -> [SrcUnion]
groupSpace _ [] = []
groupSpace text items =
    concatMap makeSpace $ foldr f [[]] items
    where
      -- Add an element to the newest list
      f x@(Space' {}) (xs : xss) = (x : xs) : xss
      f x@(Comment' {}) (xs : xss) = (x : xs) : xss
      -- Start a new list
      f _ ([] : xss) = ([] : xss)
      f _ (xs : xss) = ([] : xs : xss)
      f _ [] = error "no space"
      -- Turn a list of elements into a single Space element
      makeSpace  :: [SrcUnion] -> [SrcUnion]
      makeSpace [] = []
      makeSpace xs@(x : _) =
          let b = srcLoc x
              e = endLoc (last xs)
              sp = srcSpan (b, e) in
          [Space' (srcSpanText sp text) sp]

appendLoc :: String -> SrcLoc -> SrcLoc
appendLoc text loc =
    case lines' text of
      [line] -> loc {srcColumn = srcColumn loc + length line}
      xs -> loc {srcLine = srcLine loc + length xs - 1, srcColumn = length (last xs) + 1}

tests :: Test
tests = TestList [test2a, test2b, test2c, test2d, test2e, test2f, test2g, test1a, test1b, test3, test3b, test4, testGroupSpace]

testGroupSpace :: Test
testGroupSpace =
    TestLabel "testGroupSpace" $ TestCase $ withCurrentDirectory "testdata" $
      (readFile "Debian/Repo/AptCache.hs" >>= \ text ->
       assertEqual "groupSpace"
                [Space' "-- Flip args to get newest first\n          " (SrcSpan "<unknown>.hs" 114 33 115 11)]
                (groupSpace
                   (untabify text)
                   [Comment' (Comment
                                  False
                                  (SrcSpan {srcSpanFilename = "<unknown>.hs",
                                            srcSpanStartLine = 114, srcSpanStartColumn = 33,
                                            srcSpanEndLine = 114, srcSpanEndColumn = 65})
                                  " Flip args to get newest first"),
                    Space' "\n          " (SrcSpan "<unknown>.hs" 114 65 115 11)]))

withTestData :: (Module SrcSpanInfo-> [Comment] -> String -> r) -> IO r
withTestData f = withCurrentDirectory "testdata" $
    do let path = "Debian/Repo/AptCache.hs"
       text <- try (readFile path)
       source <- try (parseFileWithComments defaultParseMode path)
       case (text, source) of
         (Right text', Right (ParseOk (m, comments))) ->
             return $ f m comments (untabify text')
         (Right _, Right _) -> error "parse failure"
         (Left (e :: SomeException), _) -> error $ "failure: " ++ show e
         (_, Left (e :: SomeException)) -> error $ "failure: " ++ show e

-- Turn these into unit tests, and test for the md5sum of AptCache.hs (3c0c2e7422bfc3c3f39402f3dd4fa5af)
test1a :: Test
test1a =
    TestLabel "test1a" $ TestCase
    (withTestData test >>= \ output ->
     assertEqual
     "foldModule"
     ["pre","pragma: [2:1 - 3:1]","pragma: [3:1 - 7:8]","name: [7:8 - 7:29]","warning: [7:29 - 8:5]","exports: [8:5 - 34:1]",
      "import: [34:1 - 35:1]","import: [35:1 - 36:1]","import: [36:1 - 37:1]","import: [37:1 - 38:1]",
      "import: [38:1 - 39:1]","import: [39:1 - 40:1]","import: [40:1 - 41:1]","import: [41:1 - 42:1]","import: [42:1 - 43:1]",
      "import: [43:1 - 44:1]","import: [44:1 - 45:1]","import: [45:1 - 46:1]","import: [46:1 - 47:1]","import: [47:1 - 48:1]",
      "import: [48:1 - 49:1]","import: [49:1 - 50:1]","import: [50:1 - 51:1]","import: [51:1 - 52:1]","import: [52:1 - 53:1]",
      "import: [53:1 - 54:1]","import: [54:1 - 55:1]","import: [55:1 - 56:1]","import: [56:1 - 57:1]","import: [57:1 - 58:1]",
      "import: [58:1 - 59:1]","import: [59:1 - 60:1]","import: [60:1 - 61:1]","import: [61:1 - 62:1]","import: [62:1 - 63:1]",
      "import: [63:1 - 64:1]","import: [64:1 - 65:1]","import: [65:1 - 66:1]","import: [66:1 - 67:1]","import: [67:1 - 68:1]",
      "import: [68:1 - 70:1]","decl: [70:1 - 72:1]","pre","decl: [79:1 - 80:1]","decl: [80:1 - 82:1]","decl: [82:1 - 83:1]",
      "decl: [83:1 - 85:1]","decl: [85:1 - 86:1]","decl: [86:1 - 88:1]","decl: [88:1 - 89:1]","decl: [89:1 - 91:1]","pre",
      "decl: [92:1 - 93:1]","decl: [93:1 - 95:1]","decl: [95:1 - 96:1]","decl: [96:1 - 98:1]","pre","decl: [103:1 - 104:1]",
      "decl: [104:1 - 106:1]","pre","decl: [107:1 - 108:1]","decl: [108:1 - 119:1]","pre",
      "decl: [121:1 - 122:1]","decl: [122:1 - 146:1]","pre","decl: [147:1 - 148:1]","decl: [148:1 - 150:1]",
      "pre","decl: [151:1 - 152:1]","decl: [152:1 - 154:1]","pre","decl: [156:1 - 157:1]",
      "decl: [157:1 - 165:1]","decl: [165:1 - 166:1]","decl: [166:1 - 207:1]","decl: [207:1 - 208:1]","decl: [208:1 - 216:1]",
      "pre","decl: [228:1 - 229:1]","decl: [229:1 - 241:1]","pre","decl: [253:1 - 254:1]",
      "decl: [254:1 - 261:1]","decl: [261:1 - 262:1]","decl: [262:1 - 267:1]","decl: [267:1 - 273:1]","pre",
      "decl: [275:1 - 276:1]","decl: [276:1 - 329:1]","pre","decl: [330:1 - 331:1]","decl: [331:1 - 343:1]",
      "decl: [343:1 - 344:1]","decl: [344:1 - 356:1]","decl: [356:1 - 357:1]","decl: [357:1 - 361:1]","decl: [361:1 - 362:1]",
      "decl: [362:1 - 366:1]","decl: [366:1 - 367:1]","decl: [367:1 - 371:1]","decl: [371:1 - 372:1]","decl: [372:1 - 376:1]",
      "pre","decl: [377:1 - 378:1]","decl: [378:1 - 385:1]","decl: [385:1 - 386:1]","decl: [386:1 - 393:1]"]
     output)
    where
      test :: Module SrcSpanInfo -> [Comment] -> String -> [String]
      test m comments text =
          foldModule pragmaf namef warningf exportsf importf declf spacef m comments text []
          where
            pragmaf :: ModulePragma SrcSpanInfo -> String -> String -> [String] -> [String]
            pragmaf x pre _s r = r ++ (if pre /= "" then ["pre"] else []) ++ ["pragma: " ++ display (srcSpan x)]
            namef :: ModuleName SrcSpanInfo -> String -> String -> [String] -> [String]
            namef x pre _s r = r ++ (if pre /= "" then ["pre"] else []) ++ ["name: " ++ display (srcSpan x)]
            warningf :: WarningText SrcSpanInfo -> String -> String -> [String] -> [String]
            warningf x pre _s r = r ++ (if pre /= "" then ["pre"] else []) ++ ["warning: " ++ display (srcSpan x)]
            exportsf :: ExportSpecList SrcSpanInfo -> String -> String -> [String] -> [String]
            exportsf x pre _s r = r ++ (if pre /= "" then ["pre"] else []) ++ ["exports: " ++ display (srcSpan x)]
            importf :: ImportDecl SrcSpanInfo -> String -> String -> [String] -> [String]
            importf x pre _s r = r ++ (if pre /= "" then ["pre"] else []) ++ ["import: " ++ display (srcSpan x)]
            declf :: Decl SrcSpanInfo -> String -> String -> [String] -> [String]
            declf x pre _s r = r ++ (if pre /= "" then ["pre"] else []) ++ ["decl: " ++ display (srcSpan x)]
            spacef :: String -> [String] -> [String]
            spacef _s r = r ++ ["space"]

-- Turn these into unit tests, and test for the md5sum of AptCache.hs (3c0c2e7422bfc3c3f39402f3dd4fa5af)
test1b :: Test
test1b =
    TestLabel "test1b" $ TestCase
    (withTestData test >>= \ (output, original) ->
     assertEqual
     "foldModule"
     original
     output)
    where
      test :: Module SrcSpanInfo -> [Comment] -> String -> (String, String)
      test m comments text =
          (foldModule pragmaf namef warningf exportsf importf declf spacef m comments text [], text)
          where
            pragmaf :: ModulePragma SrcSpanInfo -> String -> String -> String -> String
            pragmaf _x pre s r = r ++ pre ++ s
            namef :: ModuleName SrcSpanInfo -> String -> String -> String -> String
            namef _x pre s r = r ++ pre ++ s
            warningf :: WarningText SrcSpanInfo -> String -> String -> String -> String
            warningf _x pre s r = r ++ pre ++ s
            exportsf :: ExportSpecList SrcSpanInfo -> String -> String -> String -> String
            exportsf _x pre s r = r ++ pre ++ s
            importf :: ImportDecl SrcSpanInfo -> String -> String -> String -> String
            importf _x pre s r = r ++ pre ++ s
            declf :: Decl SrcSpanInfo -> String -> String -> String -> String
            declf _x pre s r = r ++ pre ++ s
            spacef :: String -> String -> String
            spacef s r = r ++ s

test2a :: Test
test2a =
    TestLabel "test2a" $ TestCase
    (withTestData (\ _ x _ -> x) >>= \ comments ->
     assertEqual
     "comments"
     [Comment True (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 1, srcSpanStartColumn = 1, srcSpanEndLine = 1, srcSpanEndColumn = 35}) " This is a comment at the top "
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 4, srcSpanStartColumn = 1, srcSpanEndLine = 4, srcSpanEndColumn = 70}) " |An AptCache represents a local cache of a remote repository.  The"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 5, srcSpanStartColumn = 1, srcSpanEndLine = 5, srcSpanEndColumn = 64}) " cached information is usually downloaded by running \"apt-get"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 6, srcSpanStartColumn = 1, srcSpanEndLine = 6, srcSpanEndColumn = 53}) " update\", and appears in @\\/var\\/lib\\/apt\\/lists@."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 72, srcSpanStartColumn = 1, srcSpanEndLine = 72, srcSpanEndColumn = 60}) " The following are path functions which can be used while"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 73, srcSpanStartColumn = 1, srcSpanEndLine = 73, srcSpanEndColumn = 62}) " constructing instances of AptCache.  Each is followed by a"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 74, srcSpanStartColumn = 1, srcSpanEndLine = 74, srcSpanEndColumn = 69}) " corresponding function that gives the same result when applied to"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 75, srcSpanStartColumn = 1, srcSpanEndLine = 75, srcSpanEndColumn = 25}) " an AptCache instance."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 77, srcSpanStartColumn = 1, srcSpanEndLine = 77, srcSpanEndColumn = 63}) " | A directory which will hold all the cached files for this"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 78, srcSpanStartColumn = 1, srcSpanEndLine = 78, srcSpanEndColumn = 19}) " NamedSliceList."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 91, srcSpanStartColumn = 1, srcSpanEndLine = 91, srcSpanEndColumn = 55}) " | The path where a text of the SliceList is stored."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 98, srcSpanStartColumn = 1, srcSpanEndLine = 98, srcSpanEndColumn = 70}) " Additional functions which can only be used on already constructed"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 99, srcSpanStartColumn = 1, srcSpanEndLine = 99, srcSpanEndColumn = 26}) " instances of AptCache."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 101, srcSpanStartColumn = 1, srcSpanEndLine = 101, srcSpanEndColumn = 70}) " | A directory holding all files downloaded by apt-get source for a"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 102, srcSpanStartColumn = 1, srcSpanEndLine = 102, srcSpanEndColumn = 19}) " certain package"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 106, srcSpanStartColumn = 1, srcSpanEndLine = 106, srcSpanEndColumn = 59}) " |Return all the named source packages sorted by version"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 114, srcSpanStartColumn = 33, srcSpanEndLine = 114, srcSpanEndColumn = 65}) " Flip args to get newest first"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 119, srcSpanStartColumn = 1, srcSpanEndLine = 119, srcSpanEndColumn = 68}) " |Return a list of the index files that contain the packages of a"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 120, srcSpanStartColumn = 1, srcSpanEndLine = 120, srcSpanEndColumn = 10}) " slice."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 146, srcSpanStartColumn = 1, srcSpanEndLine = 146, srcSpanEndColumn = 76}) " |Return the paths in the local cache of the index files of a slice list."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 150, srcSpanStartColumn = 1, srcSpanEndLine = 150, srcSpanEndColumn = 78}) " |Return the paths in the local cache of the index files of a single slice."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 154, srcSpanStartColumn = 1, srcSpanEndLine = 154, srcSpanEndColumn = 66}) " |Return the list of files that apt-get update would write into"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 155, srcSpanStartColumn = 1, srcSpanEndLine = 155, srcSpanEndColumn = 73}) " \\/var\\/lib\\/apt\\/lists when it processed the given list of DebSource."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 179, srcSpanStartColumn = 5, srcSpanEndLine = 179, srcSpanEndColumn = 24}) " what about dist?"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 188, srcSpanStartColumn = 7, srcSpanEndLine = 188, srcSpanEndColumn = 64}) " If user is given and password is not, the user name is"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 189, srcSpanStartColumn = 7, srcSpanEndLine = 189, srcSpanEndColumn = 64}) " added to the file name.  Otherwise it is not.  Really."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 209, srcSpanStartColumn = 37, srcSpanEndLine = 209, srcSpanEndColumn = 109}) " This is required for dpkg-architecture to work in a build environment"
     ,Comment True (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 216, srcSpanStartColumn = 1, srcSpanEndLine = 226, srcSpanEndColumn = 3}) "\n  (err, _) <- useEnv root forceList (readProcessChunks (shell cmd) L.empty) >>= return . collectOutputs\n       case code of\n         (ExitSuccess : _) ->\n             case words (UTF8.toString (B.concat (L.toChunks out))) of\n               [] -> error $ \"Invalid output from \" ++ cmd\n               (arch : _) -> return (Binary arch)\n         _ -> error $ \"Failure: \" ++ cmd ++ \" -> \" ++ show code ++ \"\\n\\nstdout:\\n\\n\" ++ show out ++ \"\\n\\nstderr:\\n\\n\" ++ show err\n    where\n      cmd = \"export LOGNAME=root; dpkg-architecture -qDEB_BUILD_ARCH\"\n"
     ,Comment True (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 241, srcSpanStartColumn = 1, srcSpanEndLine = 251, srcSpanEndColumn = 3}) "\n    do (code, out, err, _) <- runProcess (shell cmd) L.empty >>= return . collectOutputs\n       case code of\n         (ExitSuccess : _) ->\n             case words (UTF8.toString (B.concat (L.toChunks out))) of\n               [] -> error $ \"Invalid output from \" ++ cmd\n               (arch : _) -> return (Binary arch)\n         _ -> error $ \"Failure: \" ++ cmd ++ \" -> \" ++ show code ++ \"\\n\\nstdout:\\n\\n\" ++ show out ++ \"\\n\\nstderr:\\n\\n\" ++ show err\n    where\n      cmd = \"dpkg-architecture -qDEB_BUILD_ARCH\"\n"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 273, srcSpanStartColumn = 1, srcSpanEndLine = 273, srcSpanEndColumn = 66}) " |Change the sources.list of an AptCache object, subject to the"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 274, srcSpanStartColumn = 1, srcSpanEndLine = 274, srcSpanEndColumn = 34}) " value of sourcesChangedAction."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 277, srcSpanStartColumn = 5, srcSpanEndLine = 277, srcSpanEndColumn = 68}) " (\\ x -> qPutStrLn \"Updating cache sources\" >> quieter 2 x) $"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 281, srcSpanStartColumn = 7, srcSpanEndLine = 281, srcSpanEndColumn = 73}) "let distro@(ReleaseCache _ dist _) = releaseFromConfig' top text"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 318, srcSpanStartColumn = 23, srcSpanEndLine = 318, srcSpanEndColumn = 72}) " The sources.list has changed, but it should be"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 319, srcSpanStartColumn = 23, srcSpanEndLine = 319, srcSpanEndColumn = 44}) " safe to update it."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 329, srcSpanStartColumn = 1, srcSpanEndLine = 329, srcSpanEndColumn = 78}) " | Return a sorted list of available source packages, newest version first."
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 338, srcSpanStartColumn = 33, srcSpanEndLine = 338, srcSpanEndColumn = 65}) " Flip args to get newest first"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 351, srcSpanStartColumn = 33, srcSpanEndLine = 351, srcSpanEndColumn = 65}) " Flip args to get newest first"
     ,Comment False (SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 376, srcSpanStartColumn = 1, srcSpanEndLine = 376, srcSpanEndColumn = 72}) " | Note that apt-get source works for binary or source package names."]
     comments)

test2b :: Test
test2b =
    TestLabel "test2b" $ TestCase
    (withTestData test >>= \ items ->
     assertEqual
     "test2b"
     ["Comment' [1:1 - 1:35]",
      "Space' [1:35 - 2:1]",
      "Other' [2:1 - 4:1]",
      "Comment' [4:1 - 4:70]",
      "Space' [4:70 - 5:1]",
      "Comment' [5:1 - 5:64]",
      "Space' [5:64 - 6:1]",
      "Comment' [6:1 - 6:53]",
      "Space' [6:53 - 7:1]",
      "Other' [7:1 - 72:1]",
      "Comment' [72:1 - 72:60]",
      "Space' [72:60 - 73:1]",
      "Comment' [73:1 - 73:62]",
      "Space' [73:62 - 74:1]",
      "Comment' [74:1 - 74:69]",
      "Space' [74:69 - 75:1]",
      "Comment' [75:1 - 75:25]",
      "Space' [75:25 - 77:1]",
      "Comment' [77:1 - 77:63]",
      "Space' [77:63 - 78:1]",
      "Comment' [78:1 - 78:19]",
      "Space' [78:19 - 79:1]",
      "Other' [79:1 - 91:1]",
      "Comment' [91:1 - 91:55]",
      "Space' [91:55 - 92:1]",
      "Other' [92:1 - 98:1]",
      "Comment' [98:1 - 98:70]",
      "Space' [98:70 - 99:1]",
      "Comment' [99:1 - 99:26]",
      "Space' [99:26 - 101:1]",
      "Comment' [101:1 - 101:70]",
      "Space' [101:70 - 102:1]",
      "Comment' [102:1 - 102:19]",
      "Space' [102:19 - 103:1]",
      "Other' [103:1 - 106:1]",
      "Comment' [106:1 - 106:59]",
      "Space' [106:59 - 107:1]",
      "Other' [107:1 - 114:33]",
      "Comment' [114:33 - 114:65]",
      "Space' [114:65 - 115:11]",
      "Other' [115:11 - 119:1]",
      "Comment' [119:1 - 119:68]",
      "Space' [119:68 - 120:1]",
      "Comment' [120:1 - 120:10]",
      "Space' [120:10 - 121:1]",
      "Other' [121:1 - 146:1]",
      "Comment' [146:1 - 146:76]",
      "Space' [146:76 - 147:1]",
      "Other' [147:1 - 150:1]",
      "Comment' [150:1 - 150:78]",
      "Space' [150:78 - 151:1]",
      "Other' [151:1 - 154:1]",
      "Comment' [154:1 - 154:66]",
      "Space' [154:66 - 155:1]",
      "Comment' [155:1 - 155:73]",
      "Space' [155:73 - 156:1]",
      "Other' [156:1 - 179:5]",
      "Comment' [179:5 - 179:24]",
      "Space' [179:24 - 180:5]",
      "Other' [180:5 - 188:7]",
      "Comment' [188:7 - 188:64]",
      "Space' [188:64 - 189:7]",
      "Comment' [189:7 - 189:64]",
      "Space' [189:64 - 190:7]",
      "Other' [190:7 - 209:37]",
      "Comment' [209:37 - 209:109]",
      "Space' [209:109 - 210:8]",
      "Other' [210:8 - 216:1]",
      "Comment' [216:1 - 226:3]",
      "Space' [226:3 - 228:1]",
      "Other' [228:1 - 241:1]",
      "Comment' [241:1 - 251:3]",
      "Space' [251:3 - 253:1]",
      "Other' [253:1 - 273:1]",
      "Comment' [273:1 - 273:66]",
      "Space' [273:66 - 274:1]",
      "Comment' [274:1 - 274:34]",
      "Space' [274:34 - 275:1]",
      "Other' [275:1 - 277:5]",
      "Comment' [277:5 - 277:68]",
      "Space' [277:68 - 278:5]",
      "Other' [278:5 - 281:7]",
      "Comment' [281:7 - 281:73]",
      "Space' [281:73 - 282:7]",
      "Other' [282:7 - 318:23]",
      "Comment' [318:23 - 318:72]",
      "Space' [318:72 - 319:23]",
      "Comment' [319:23 - 319:44]",
      "Space' [319:44 - 320:23]",
      "Other' [320:23 - 329:1]",
      "Comment' [329:1 - 329:78]",
      "Space' [329:78 - 330:1]",
      "Other' [330:1 - 338:33]",
      "Comment' [338:33 - 338:65]",
      "Space' [338:65 - 339:11]",
      "Other' [339:11 - 351:33]",
      "Comment' [351:33 - 351:65]",
      "Space' [351:65 - 352:11]",
      "Other' [352:11 - 376:1]",
      "Comment' [376:1 - 376:72]",
      "Space' [376:72 - 377:1]",
      "Other' [377:1 - 393:1]"]
     (map display items))
    where
      test :: Module SrcSpanInfo -> [Comment] -> String -> [SrcUnion]
      test _ comments text =
          insertSpaceItems text $ sortBy (compare `on` srcLoc) (map Comment' comments)

test2c :: Test
test2c =
    TestLabel "test2c" $ TestCase
    (withTestData test >>= \ items ->
     assertEqual
     "moduleDecls"
     ["Pragma' [2:1 - 3:1]","Pragma' [3:1 - 7:8]", "Name' [7:8 - 7:29]", "Warning' [7:29 - 8:5]", "Exports' [8:5 - 34:1]","ImportDecl' [34:1 - 35:1]","ImportDecl' [35:1 - 36:1]","ImportDecl' [36:1 - 37:1]","ImportDecl' [37:1 - 38:1]","ImportDecl' [38:1 - 39:1]","ImportDecl' [39:1 - 40:1]","ImportDecl' [40:1 - 41:1]","ImportDecl' [41:1 - 42:1]","ImportDecl' [42:1 - 43:1]","ImportDecl' [43:1 - 44:1]","ImportDecl' [44:1 - 45:1]","ImportDecl' [45:1 - 46:1]","ImportDecl' [46:1 - 47:1]","ImportDecl' [47:1 - 48:1]","ImportDecl' [48:1 - 49:1]","ImportDecl' [49:1 - 50:1]","ImportDecl' [50:1 - 51:1]","ImportDecl' [51:1 - 52:1]","ImportDecl' [52:1 - 53:1]","ImportDecl' [53:1 - 54:1]","ImportDecl' [54:1 - 55:1]","ImportDecl' [55:1 - 56:1]","ImportDecl' [56:1 - 57:1]","ImportDecl' [57:1 - 58:1]","ImportDecl' [58:1 - 59:1]","ImportDecl' [59:1 - 60:1]","ImportDecl' [60:1 - 61:1]","ImportDecl' [61:1 - 62:1]","ImportDecl' [62:1 - 63:1]","ImportDecl' [63:1 - 64:1]","ImportDecl' [64:1 - 65:1]","ImportDecl' [65:1 - 66:1]","ImportDecl' [66:1 - 67:1]","ImportDecl' [67:1 - 68:1]","ImportDecl' [68:1 - 70:1]","Decl' [70:1 - 79:1]","Decl' [79:1 - 80:1]","Decl' [80:1 - 82:1]","Decl' [82:1 - 83:1]","Decl' [83:1 - 85:1]","Decl' [85:1 - 86:1]","Decl' [86:1 - 88:1]","Decl' [88:1 - 89:1]","Decl' [89:1 - 92:1]","Decl' [92:1 - 93:1]","Decl' [93:1 - 95:1]","Decl' [95:1 - 96:1]","Decl' [96:1 - 103:1]","Decl' [103:1 - 104:1]","Decl' [104:1 - 107:1]","Decl' [107:1 - 108:1]","Decl' [108:1 - 121:1]","Decl' [121:1 - 122:1]","Decl' [122:1 - 147:1]","Decl' [147:1 - 148:1]","Decl' [148:1 - 151:1]","Decl' [151:1 - 152:1]","Decl' [152:1 - 156:1]","Decl' [156:1 - 157:1]","Decl' [157:1 - 165:1]","Decl' [165:1 - 166:1]","Decl' [166:1 - 207:1]","Decl' [207:1 - 208:1]","Decl' [208:1 - 228:1]","Decl' [228:1 - 229:1]","Decl' [229:1 - 253:1]","Decl' [253:1 - 254:1]","Decl' [254:1 - 261:1]","Decl' [261:1 - 262:1]","Decl' [262:1 - 267:1]","Decl' [267:1 - 275:1]","Decl' [275:1 - 276:1]","Decl' [276:1 - 330:1]","Decl' [330:1 - 331:1]","Decl' [331:1 - 343:1]","Decl' [343:1 - 344:1]","Decl' [344:1 - 356:1]","Decl' [356:1 - 357:1]","Decl' [357:1 - 361:1]","Decl' [361:1 - 362:1]","Decl' [362:1 - 366:1]","Decl' [366:1 - 367:1]","Decl' [367:1 - 371:1]","Decl' [371:1 - 372:1]","Decl' [372:1 - 377:1]","Decl' [377:1 - 378:1]","Decl' [378:1 - 385:1]","Decl' [385:1 - 386:1]","Decl' [386:1 - 393:1]"]
     (map display items))
    where
      test :: Module SrcSpanInfo -> [Comment] -> String -> [SrcUnion]
      test m _ text = moduleDecls text m

test2d :: Test
test2d =
    TestLabel "test2d" $ TestCase
    (withTestData test >>= \ items ->
     assertEqual
     "moduleItems"
     ["Decl' [386:1 - 393:1]","Decl' [385:1 - 386:1]","Decl' [378:1 - 385:1]","Decl' [377:1 - 378:1]","Space' [376:1 - 377:1]","Decl' [372:1 - 377:1]","Decl' [371:1 - 372:1]","Decl' [367:1 - 371:1]","Decl' [366:1 - 367:1]","Decl' [362:1 - 366:1]","Decl' [361:1 - 362:1]","Decl' [357:1 - 361:1]","Decl' [356:1 - 357:1]","Decl' [344:1 - 356:1]","Space' [351:33 - 352:11]","Decl' [343:1 - 344:1]","Decl' [331:1 - 343:1]","Space' [338:33 - 339:11]","Decl' [330:1 - 331:1]","Space' [329:1 - 330:1]","Decl' [276:1 - 330:1]","Space' [318:23 - 320:23]","Space' [281:7 - 282:7]","Space' [277:5 - 278:5]","Decl' [275:1 - 276:1]","Space' [273:1 - 275:1]","Decl' [267:1 - 275:1]","Decl' [262:1 - 267:1]","Decl' [261:1 - 262:1]","Decl' [254:1 - 261:1]","Decl' [253:1 - 254:1]","Space' [241:1 - 253:1]","Decl' [229:1 - 253:1]","Decl' [228:1 - 229:1]","Space' [216:1 - 228:1]","Decl' [208:1 - 228:1]","Space' [209:37 - 210:8]","Decl' [207:1 - 208:1]","Decl' [166:1 - 207:1]","Space' [188:7 - 190:7]","Space' [179:5 - 180:5]","Decl' [165:1 - 166:1]","Decl' [157:1 - 165:1]","Decl' [156:1 - 157:1]","Space' [154:1 - 156:1]","Decl' [152:1 - 156:1]","Decl' [151:1 - 152:1]","Space' [150:1 - 151:1]","Decl' [148:1 - 151:1]","Decl' [147:1 - 148:1]","Space' [146:1 - 147:1]","Decl' [122:1 - 147:1]","Decl' [121:1 - 122:1]","Space' [119:1 - 121:1]","Decl' [108:1 - 121:1]","Space' [114:33 - 115:11]","Decl' [107:1 - 108:1]","Space' [106:1 - 107:1]","Decl' [104:1 - 107:1]","Decl' [103:1 - 104:1]","Space' [98:1 - 103:1]","Decl' [96:1 - 103:1]","Decl' [95:1 - 96:1]","Decl' [93:1 - 95:1]","Decl' [92:1 - 93:1]","Space' [91:1 - 92:1]","Decl' [89:1 - 92:1]","Decl' [88:1 - 89:1]","Decl' [86:1 - 88:1]","Decl' [85:1 - 86:1]","Decl' [83:1 - 85:1]","Decl' [82:1 - 83:1]","Decl' [80:1 - 82:1]","Decl' [79:1 - 80:1]","Space' [72:1 - 79:1]","Decl' [70:1 - 79:1]","ImportDecl' [68:1 - 70:1]","ImportDecl' [67:1 - 68:1]","ImportDecl' [66:1 - 67:1]","ImportDecl' [65:1 - 66:1]","ImportDecl' [64:1 - 65:1]","ImportDecl' [63:1 - 64:1]","ImportDecl' [62:1 - 63:1]","ImportDecl' [61:1 - 62:1]","ImportDecl' [60:1 - 61:1]","ImportDecl' [59:1 - 60:1]","ImportDecl' [58:1 - 59:1]","ImportDecl' [57:1 - 58:1]","ImportDecl' [56:1 - 57:1]","ImportDecl' [55:1 - 56:1]","ImportDecl' [54:1 - 55:1]","ImportDecl' [53:1 - 54:1]","ImportDecl' [52:1 - 53:1]","ImportDecl' [51:1 - 52:1]","ImportDecl' [50:1 - 51:1]","ImportDecl' [49:1 - 50:1]","ImportDecl' [48:1 - 49:1]","ImportDecl' [47:1 - 48:1]","ImportDecl' [46:1 - 47:1]","ImportDecl' [45:1 - 46:1]","ImportDecl' [44:1 - 45:1]","ImportDecl' [43:1 - 44:1]","ImportDecl' [42:1 - 43:1]","ImportDecl' [41:1 - 42:1]","ImportDecl' [40:1 - 41:1]","ImportDecl' [39:1 - 40:1]","ImportDecl' [38:1 - 39:1]","ImportDecl' [37:1 - 38:1]","ImportDecl' [36:1 - 37:1]","ImportDecl' [35:1 - 36:1]","ImportDecl' [34:1 - 35:1]","Exports' [8:5 - 34:1]","Warning' [7:29 - 8:5]","Name' [7:8 - 7:29]","Pragma' [3:1 - 7:8]","Space' [4:1 - 7:1]","Pragma' [2:1 - 3:1]","Space' [1:1 - 2:1]"]
     (map display items))
    where
      test :: Module SrcSpanInfo -> [Comment] -> String -> [SrcUnion]
      test m comments text = moduleItems text m comments
      moduleItems :: String -> Module SrcSpanInfo -> [Comment] -> [SrcUnion]
      moduleItems text m comments =
          descendingEnds
          where
            descendingEnds = reverse $ sortBy (compare `on` endLoc) $ decls ++ space
            decls = moduleDecls text m
            space = moduleSpace text comments

test2e :: Test
test2e =
    TestLabel "test2e" $ TestCase
    (withTestData test >>= \ items ->
     assertEqual
     "moduleItemGroups"
     [["Decl' [386:1 - 393:1]"],
      ["Decl' [385:1 - 386:1]"],
      ["Decl' [378:1 - 385:1]"],
      ["Decl' [377:1 - 378:1]"],
      ["Decl' [372:1 - 377:1]","Space' [376:1 - 377:1]"],
      ["Decl' [371:1 - 372:1]"],
      ["Decl' [367:1 - 371:1]"],
      ["Decl' [366:1 - 367:1]"],
      ["Decl' [362:1 - 366:1]"],
      ["Decl' [361:1 - 362:1]"],
      ["Decl' [357:1 - 361:1]"],
      ["Decl' [356:1 - 357:1]"],
      ["Space' [351:33 - 352:11]","Decl' [344:1 - 356:1]"],
      ["Decl' [343:1 - 344:1]"],
      ["Space' [338:33 - 339:11]","Decl' [331:1 - 343:1]"],
      ["Decl' [330:1 - 331:1]"],
      ["Space' [277:5 - 278:5]","Space' [281:7 - 282:7]","Space' [318:23 - 320:23]","Decl' [276:1 - 330:1]","Space' [329:1 - 330:1]"],
      ["Decl' [275:1 - 276:1]"],
      ["Decl' [267:1 - 275:1]","Space' [273:1 - 275:1]"],
      ["Decl' [262:1 - 267:1]"],
      ["Decl' [261:1 - 262:1]"],
      ["Decl' [254:1 - 261:1]"],
      ["Decl' [253:1 - 254:1]"],
      ["Decl' [229:1 - 253:1]","Space' [241:1 - 253:1]"],
      ["Decl' [228:1 - 229:1]"],
      ["Space' [209:37 - 210:8]","Decl' [208:1 - 228:1]","Space' [216:1 - 228:1]"],
      ["Decl' [207:1 - 208:1]"],
      ["Space' [179:5 - 180:5]","Space' [188:7 - 190:7]","Decl' [166:1 - 207:1]"],
      ["Decl' [165:1 - 166:1]"],
      ["Decl' [157:1 - 165:1]"],
      ["Decl' [156:1 - 157:1]"],
      ["Decl' [152:1 - 156:1]","Space' [154:1 - 156:1]"],
      ["Decl' [151:1 - 152:1]"],
      ["Decl' [148:1 - 151:1]","Space' [150:1 - 151:1]"],
      ["Decl' [147:1 - 148:1]"],
      ["Decl' [122:1 - 147:1]","Space' [146:1 - 147:1]"],
      ["Decl' [121:1 - 122:1]"],
      ["Space' [114:33 - 115:11]","Decl' [108:1 - 121:1]","Space' [119:1 - 121:1]"],
      ["Decl' [107:1 - 108:1]"],
      ["Decl' [104:1 - 107:1]","Space' [106:1 - 107:1]"],
      ["Decl' [103:1 - 104:1]"],
      ["Decl' [96:1 - 103:1]","Space' [98:1 - 103:1]"],
      ["Decl' [95:1 - 96:1]"],
      ["Decl' [93:1 - 95:1]"],
      ["Decl' [92:1 - 93:1]"],
      ["Decl' [89:1 - 92:1]","Space' [91:1 - 92:1]"],
      ["Decl' [88:1 - 89:1]"],
      ["Decl' [86:1 - 88:1]"],
      ["Decl' [85:1 - 86:1]"],
      ["Decl' [83:1 - 85:1]"],
      ["Decl' [82:1 - 83:1]"],
      ["Decl' [80:1 - 82:1]"],
      ["Decl' [79:1 - 80:1]"],
      ["Decl' [70:1 - 79:1]","Space' [72:1 - 79:1]"],
      ["ImportDecl' [68:1 - 70:1]"],
      ["ImportDecl' [67:1 - 68:1]"],
      ["ImportDecl' [66:1 - 67:1]"],
      ["ImportDecl' [65:1 - 66:1]"],
      ["ImportDecl' [64:1 - 65:1]"],
      ["ImportDecl' [63:1 - 64:1]"],
      ["ImportDecl' [62:1 - 63:1]"],
      ["ImportDecl' [61:1 - 62:1]"],
      ["ImportDecl' [60:1 - 61:1]"],
      ["ImportDecl' [59:1 - 60:1]"],
      ["ImportDecl' [58:1 - 59:1]"],
      ["ImportDecl' [57:1 - 58:1]"],
      ["ImportDecl' [56:1 - 57:1]"],
      ["ImportDecl' [55:1 - 56:1]"],
      ["ImportDecl' [54:1 - 55:1]"],
      ["ImportDecl' [53:1 - 54:1]"],
      ["ImportDecl' [52:1 - 53:1]"],
      ["ImportDecl' [51:1 - 52:1]"],
      ["ImportDecl' [50:1 - 51:1]"],
      ["ImportDecl' [49:1 - 50:1]"],
      ["ImportDecl' [48:1 - 49:1]"],
      ["ImportDecl' [47:1 - 48:1]"],
      ["ImportDecl' [46:1 - 47:1]"],
      ["ImportDecl' [45:1 - 46:1]"],
      ["ImportDecl' [44:1 - 45:1]"],
      ["ImportDecl' [43:1 - 44:1]"],
      ["ImportDecl' [42:1 - 43:1]"],
      ["ImportDecl' [41:1 - 42:1]"],
      ["ImportDecl' [40:1 - 41:1]"],
      ["ImportDecl' [39:1 - 40:1]"],
      ["ImportDecl' [38:1 - 39:1]"],
      ["ImportDecl' [37:1 - 38:1]"],
      ["ImportDecl' [36:1 - 37:1]"],
      ["ImportDecl' [35:1 - 36:1]"],
      ["ImportDecl' [34:1 - 35:1]"],
      ["Exports' [8:5 - 34:1]"],
      ["Warning' [7:29 - 8:5]"],
      ["Name' [7:8 - 7:29]"],
      ["Space' [4:1 - 7:1]","Pragma' [3:1 - 7:8]"],
      ["Pragma' [2:1 - 3:1]"],
      ["Space' [1:1 - 2:1]"]
     ]
     (map (map display) items))
    where
      test :: Module SrcSpanInfo -> [Comment] -> String -> [[SrcUnion]]
      test m comments text = moduleItemGroups text m comments

test2f :: Test
test2f =
    TestLabel "test2f" $ TestCase
    (withTestData test >>= \ items ->
     assertEqual
     "moduleItemGroups filtered"
     [["Decl' [386:1 - 393:1]"],
      ["Decl' [385:1 - 386:1]"],
      ["Decl' [378:1 - 385:1]"],
      ["Decl' [377:1 - 378:1]"],
      ["Decl' [372:1 - 377:1]","Space' [376:1 - 377:1]"],
      ["Decl' [371:1 - 372:1]"],
      ["Decl' [367:1 - 371:1]"],
      ["Decl' [366:1 - 367:1]"],
      ["Decl' [362:1 - 366:1]"],
      ["Decl' [361:1 - 362:1]"],
      ["Decl' [357:1 - 361:1]"],
      ["Decl' [356:1 - 357:1]"],
      ["Decl' [344:1 - 356:1]"],
      ["Decl' [343:1 - 344:1]"],
      ["Decl' [331:1 - 343:1]"],
      ["Decl' [330:1 - 331:1]"],
      ["Decl' [276:1 - 330:1]","Space' [329:1 - 330:1]"],
      ["Decl' [275:1 - 276:1]"],
      ["Decl' [267:1 - 275:1]","Space' [273:1 - 275:1]"],
      ["Decl' [262:1 - 267:1]"],
      ["Decl' [261:1 - 262:1]"],
      ["Decl' [254:1 - 261:1]"],
      ["Decl' [253:1 - 254:1]"],
      ["Decl' [229:1 - 253:1]","Space' [241:1 - 253:1]"],
      ["Decl' [228:1 - 229:1]"],
      ["Decl' [208:1 - 228:1]","Space' [216:1 - 228:1]"],
      ["Decl' [207:1 - 208:1]"],
      ["Decl' [166:1 - 207:1]"],
      ["Decl' [165:1 - 166:1]"],
      ["Decl' [157:1 - 165:1]"],
      ["Decl' [156:1 - 157:1]"],
      ["Decl' [152:1 - 156:1]","Space' [154:1 - 156:1]"],
      ["Decl' [151:1 - 152:1]"],
      ["Decl' [148:1 - 151:1]","Space' [150:1 - 151:1]"],
      ["Decl' [147:1 - 148:1]"],
      ["Decl' [122:1 - 147:1]","Space' [146:1 - 147:1]"],
      ["Decl' [121:1 - 122:1]"],
      ["Decl' [108:1 - 121:1]","Space' [119:1 - 121:1]"],
      ["Decl' [107:1 - 108:1]"],
      ["Decl' [104:1 - 107:1]","Space' [106:1 - 107:1]"],
      ["Decl' [103:1 - 104:1]"],
      ["Decl' [96:1 - 103:1]","Space' [98:1 - 103:1]"],
      ["Decl' [95:1 - 96:1]"],
      ["Decl' [93:1 - 95:1]"],
      ["Decl' [92:1 - 93:1]"],
      ["Decl' [89:1 - 92:1]","Space' [91:1 - 92:1]"],
      ["Decl' [88:1 - 89:1]"],
      ["Decl' [86:1 - 88:1]"],
      ["Decl' [85:1 - 86:1]"],
      ["Decl' [83:1 - 85:1]"],
      ["Decl' [82:1 - 83:1]"],
      ["Decl' [80:1 - 82:1]"],
      ["Decl' [79:1 - 80:1]"],
      ["Decl' [70:1 - 79:1]","Space' [72:1 - 79:1]"],
      ["ImportDecl' [68:1 - 70:1]"],
      ["ImportDecl' [67:1 - 68:1]"],
      ["ImportDecl' [66:1 - 67:1]"],
      ["ImportDecl' [65:1 - 66:1]"],
      ["ImportDecl' [64:1 - 65:1]"],
      ["ImportDecl' [63:1 - 64:1]"],
      ["ImportDecl' [62:1 - 63:1]"],
      ["ImportDecl' [61:1 - 62:1]"],
      ["ImportDecl' [60:1 - 61:1]"],
      ["ImportDecl' [59:1 - 60:1]"],
      ["ImportDecl' [58:1 - 59:1]"],
      ["ImportDecl' [57:1 - 58:1]"],
      ["ImportDecl' [56:1 - 57:1]"],
      ["ImportDecl' [55:1 - 56:1]"],
      ["ImportDecl' [54:1 - 55:1]"],
      ["ImportDecl' [53:1 - 54:1]"],
      ["ImportDecl' [52:1 - 53:1]"],
      ["ImportDecl' [51:1 - 52:1]"],
      ["ImportDecl' [50:1 - 51:1]"],
      ["ImportDecl' [49:1 - 50:1]"],
      ["ImportDecl' [48:1 - 49:1]"],
      ["ImportDecl' [47:1 - 48:1]"],
      ["ImportDecl' [46:1 - 47:1]"],
      ["ImportDecl' [45:1 - 46:1]"],
      ["ImportDecl' [44:1 - 45:1]"],
      ["ImportDecl' [43:1 - 44:1]"],
      ["ImportDecl' [42:1 - 43:1]"],
      ["ImportDecl' [41:1 - 42:1]"],
      ["ImportDecl' [40:1 - 41:1]"],
      ["ImportDecl' [39:1 - 40:1]"],
      ["ImportDecl' [38:1 - 39:1]"],
      ["ImportDecl' [37:1 - 38:1]"],
      ["ImportDecl' [36:1 - 37:1]"],
      ["ImportDecl' [35:1 - 36:1]"],
      ["ImportDecl' [34:1 - 35:1]"],
      ["Exports' [8:5 - 34:1]"],
      ["Warning' [7:29 - 8:5]"],
      ["Name' [7:8 - 7:29]"],
      ["Pragma' [3:1 - 7:8]"],
      ["Pragma' [2:1 - 3:1]"],
      ["Space' [1:1 - 2:1]"]
     ]
     (map (map display) items))
    where
      test :: Module SrcSpanInfo -> [Comment] -> String -> [[SrcUnion]]
      test m comments text =
          map filterEmbedded $ moduleItemGroups text m comments

test2g :: Test
test2g =
    TestLabel "test2g" $ TestCase
    (withTestData test >>= \ items ->
     assertEqual
     "moduleItemsFinal"
     ["Space' [1:1 - 2:1]",
      "Pragma' [2:1 - 3:1]",
      "Pragma' [3:1 - 7:8]",
      "Name' [7:8 - 7:29]",
      "Warning' [7:29 - 8:5]",
      "Exports' [8:5 - 34:1]",
      "ImportDecl' [34:1 - 35:1]",
      "ImportDecl' [35:1 - 36:1]",
      "ImportDecl' [36:1 - 37:1]",
      "ImportDecl' [37:1 - 38:1]",
      "ImportDecl' [38:1 - 39:1]",
      "ImportDecl' [39:1 - 40:1]",
      "ImportDecl' [40:1 - 41:1]",
      "ImportDecl' [41:1 - 42:1]",
      "ImportDecl' [42:1 - 43:1]",
      "ImportDecl' [43:1 - 44:1]",
      "ImportDecl' [44:1 - 45:1]",
      "ImportDecl' [45:1 - 46:1]",
      "ImportDecl' [46:1 - 47:1]",
      "ImportDecl' [47:1 - 48:1]",
      "ImportDecl' [48:1 - 49:1]",
      "ImportDecl' [49:1 - 50:1]",
      "ImportDecl' [50:1 - 51:1]",
      "ImportDecl' [51:1 - 52:1]",
      "ImportDecl' [52:1 - 53:1]",
      "ImportDecl' [53:1 - 54:1]",
      "ImportDecl' [54:1 - 55:1]",
      "ImportDecl' [55:1 - 56:1]",
      "ImportDecl' [56:1 - 57:1]",
      "ImportDecl' [57:1 - 58:1]",
      "ImportDecl' [58:1 - 59:1]",
      "ImportDecl' [59:1 - 60:1]",
      "ImportDecl' [60:1 - 61:1]",
      "ImportDecl' [61:1 - 62:1]",
      "ImportDecl' [62:1 - 63:1]",
      "ImportDecl' [63:1 - 64:1]",
      "ImportDecl' [64:1 - 65:1]",
      "ImportDecl' [65:1 - 66:1]",
      "ImportDecl' [66:1 - 67:1]",
      "ImportDecl' [67:1 - 68:1]",
      "ImportDecl' [68:1 - 70:1]",
      "Decl' [70:1 - 72:1]",
      "Space' [72:1 - 79:1]",
      "Decl' [79:1 - 80:1]",
      "Decl' [80:1 - 82:1]",
      "Decl' [82:1 - 83:1]",
      "Decl' [83:1 - 85:1]",
      "Decl' [85:1 - 86:1]",
      "Decl' [86:1 - 88:1]",
      "Decl' [88:1 - 89:1]",
      "Decl' [89:1 - 91:1]",
      "Space' [91:1 - 92:1]",
      "Decl' [92:1 - 93:1]",
      "Decl' [93:1 - 95:1]",
      "Decl' [95:1 - 96:1]",
      "Decl' [96:1 - 98:1]",
      "Space' [98:1 - 103:1]",
      "Decl' [103:1 - 104:1]",
      "Decl' [104:1 - 106:1]",
      "Space' [106:1 - 107:1]",
      "Decl' [107:1 - 108:1]",
      "Decl' [108:1 - 119:1]",
      "Space' [119:1 - 121:1]",
      "Decl' [121:1 - 122:1]",
      "Decl' [122:1 - 146:1]",
      "Space' [146:1 - 147:1]",
      "Decl' [147:1 - 148:1]",
      "Decl' [148:1 - 150:1]",
      "Space' [150:1 - 151:1]",
      "Decl' [151:1 - 152:1]",
      "Decl' [152:1 - 154:1]",
      "Space' [154:1 - 156:1]",
      "Decl' [156:1 - 157:1]",
      "Decl' [157:1 - 165:1]",
      "Decl' [165:1 - 166:1]",
      "Decl' [166:1 - 207:1]",
      "Decl' [207:1 - 208:1]",
      "Decl' [208:1 - 216:1]",
      "Space' [216:1 - 228:1]",
      "Decl' [228:1 - 229:1]",
      "Decl' [229:1 - 241:1]",
      "Space' [241:1 - 253:1]",
      "Decl' [253:1 - 254:1]",
      "Decl' [254:1 - 261:1]",
      "Decl' [261:1 - 262:1]",
      "Decl' [262:1 - 267:1]",
      "Decl' [267:1 - 273:1]",
      "Space' [273:1 - 275:1]",
      "Decl' [275:1 - 276:1]",
      "Decl' [276:1 - 329:1]",
      "Space' [329:1 - 330:1]",
      "Decl' [330:1 - 331:1]",
      "Decl' [331:1 - 343:1]",
      "Decl' [343:1 - 344:1]",
      "Decl' [344:1 - 356:1]",
      "Decl' [356:1 - 357:1]",
      "Decl' [357:1 - 361:1]",
      "Decl' [361:1 - 362:1]",
      "Decl' [362:1 - 366:1]",
      "Decl' [366:1 - 367:1]",
      "Decl' [367:1 - 371:1]",
      "Decl' [371:1 - 372:1]",
      "Decl' [372:1 - 376:1]",
      "Space' [376:1 - 377:1]",
      "Decl' [377:1 - 378:1]",
      "Decl' [378:1 - 385:1]",
      "Decl' [385:1 - 386:1]",
      "Decl' [386:1 - 393:1]"]
     (map display items))
    where
      test :: Module SrcSpanInfo -> [Comment] -> String -> [SrcUnion]
      test m comments s =
          moduleItemsFinal s m comments

test3 :: Test
test3 = TestLabel "test3" $ TestCase (assertEqual "textEndLoc" (SrcLoc {srcFilename = "<unknown>.hs", srcLine = 3, srcColumn = 1}) (textEndLoc "hello\nworld\n"))

test3b :: Test
test3b = TestLabel "test3b" $
         TestCase (withCurrentDirectory "testdata" $
                   readFile "Debian/Repo/AptCache.hs" >>= \ text ->
                   assertEqual
                   "textEndLoc2"
                   (SrcLoc {srcFilename = "<unknown>.hs", srcLine = 393, srcColumn = 1})
                   (textEndLoc text))

test4 :: Test
test4 =
    TestLabel "test4" $ TestCase $ withCurrentDirectory "testdata" $
    (B.readFile "Debian/Repo/AptCache.hs" >>= return . show . md5 >>= \ checksum ->
     assertEqual
     "Checksum"
     "520a43405fbc25fda0788a2f4607ffdc"
     checksum)
