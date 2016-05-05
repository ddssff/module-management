{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}
module Fold where

import Data.Foldable (fold)
import Data.Monoid (Monoid(mempty))
import Data.Sequence as Seq (filter, fromList, Seq, zip, (|>))
import Data.Set as Set (fromList)
import Data.Tree (Tree(..))
import qualified Language.Haskell.Exts.Annotated as A
import Language.Haskell.Exts.Annotated.Syntax ({- Eq Module -})
import qualified Language.Haskell.Exts.Parser as Exts
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldModule, ModuleInfo(..))
import Language.Haskell.Modules.ModuVerse (parseModule, runModuVerseT)
import Language.Haskell.Modules.SourceDirs (pathKey, APath(..))
import Language.Haskell.Modules.SrcLoc (HasSpanInfo(..), makeTree)
import Test.HUnit (assertEqual, Test(TestList, TestCase, TestLabel))

deriving instance Eq (Exts.ParseResult (A.Module SrcSpanInfo, [A.Comment]))
deriving instance Show (Exts.ParseMode)

tests :: Test
tests = TestLabel "Clean" (TestList [{-test1, test1b,-} test3, fold3b, fold3c, test4, test5, {-test5b,-} test6, test7])

test1 :: Test
test1 =
    TestLabel "test1" $ TestCase $ withCurrentDirectory "testdata/debian" $
    do let path = APath "Debian/Repo/Slice.hs"
       mi <- runModuVerseT $ pathKey path >>= parseModule
       let (output, original) = test mi
       assertEqual "echo" original output
    where
      test :: ModuleInfo -> (String, String)
      test m@(ModuleInfo _ text _ _ _) = (fold (foldModule echo2 echo echo echo echo2 echo echo2 echo echo echo2 m mempty), text)

test1b :: Test
test1b =
    TestLabel "test1b" $ TestCase $ withCurrentDirectory "testdata/debian" $
    do let path = APath "Debian/Repo/Slice.hs"
       mi <- runModuVerseT $ pathKey path >>= parseModule
       let output = parseTestOutput mi
       assertEqual "echo" mempty (Seq.filter (\ (a, b) -> a /= b) (Seq.zip expected output))
    where
      expected :: Seq (String, String, String, String)
      expected = Seq.fromList
                   [("","","",""),
                    ("","{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, PackageImports, StandaloneDeriving, TupleSections #-}","\n","[1.1:1.106]"),
                    ("-- |Types that represent a \"slice\" of a repository, as defined by a\n-- list of DebSource.  This is called a slice because some sections\n-- may be omitted, and because different repositories may be combined\n-- in the list.\nmodule ","Debian.Repo.Slice","\n","[6.8:6.25]"),
                    ("    ( ","","",""),
                    ("","Slice(..)","\n","[7.7:7.16]"),
                    ("    , ","SliceList(..)","\n","[8.7:8.20]"),
                    ("    , ","NamedSliceList(..)","\n","[9.7:9.25]"),
                    ("    , ","sourceSlices","\n","[10.7:10.19]"),
                    ("    , ","binarySlices","\n","[11.7:11.19]"),
                    ("    , ","inexactPathSlices","\n","[12.7:12.24]"),
                    ("    , ","releaseSlices","\n","[13.7:13.20]"),
                    ("    , ","appendSliceLists","\n","[14.7:14.23]"),
                    ("    , ","UpdateError(..)","\n","[15.7:15.22]"),
                    ("    , ","SourcesChangedAction(..)","\n","[16.7:16.31]"),
                    ("    , ","doSourcesChangedAction","\n","[17.7:17.29]"),
                    ("    ) where","","",""),
                    ("\n\n","import Control.Exception (Exception)","\n","[20.1:20.37]"),
                    ("","import Data.Data (Data)","\n","[21.1:21.24]"),
                    ("","import Data.List (intersperse)","\n","[22.1:22.31]"),
                    ("","import Data.Typeable (Typeable)","\n","[23.1:23.32]"),
                    ("","import Debian.Pretty (PP(..), ppDisplay, ppPrint)","\n","[24.1:24.50]"),
                    ("","import Debian.Release (ReleaseName(relName))","\n","[25.1:25.45]"),
                    ("","import Debian.Repo.Prelude (replaceFile)","\n","[26.1:26.41]"),
                    ("","import Debian.Repo.Prelude.Verbosity (ePutStr, ePutStrLn)","\n","[27.1:27.58]"),
                    ("","import Debian.Repo.Repo (RepoKey)","\n","[28.1:28.34]"),
                    ("","import Debian.Sources (DebSource(..), SliceName, SourceType(..))","\n","[29.1:29.65]"),
                    ("","import System.Directory (createDirectoryIfMissing, removeFile)","\n","[30.1:30.63]"),
                    ("","import System.IO (hGetLine, stdin)","\n","[31.1:31.35]"),
                    ("","import System.Unix.Directory (removeRecursiveSafely)","\n","[32.1:32.53]"),
                    ("","import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), hcat, text)","\n","[33.1:33.67]"),
                    ("\n","data Slice = Slice {sliceRepoKey :: RepoKey, sliceSource :: DebSource} deriving (Eq, Ord, Show)","\n","[35.1:35.96]"),
                    ("\n-- | Each line of the sources.list represents a slice of a repository\n","data SliceList = SliceList {slices :: [Slice]} deriving (Eq, Ord, Show)","\n","[38.1:38.72]"),
                    ("\n","data NamedSliceList\n    = NamedSliceList { sliceList :: SliceList\n                     , sliceListName :: SliceName\n                     } deriving (Eq, Ord, Show)","\n","[40.1:43.48]"),
                    ("\n","instance Pretty (PP SliceList) where\n    pPrint = hcat . intersperse (text \"\\n\") . map (ppPrint . sliceSource) . slices . unPP","\n","[45.1:48.0]"),
                    ("\n","instance Pretty (PP NamedSliceList) where\n    pPrint = ppPrint . sliceList . unPP","\n","[48.1:51.0]"),
                    ("\n","instance Pretty (PP ReleaseName) where\n    pPrint = ppPrint . relName . unPP","\n","[51.1:54.0]"),
                    ("\n","deriving instance Show SourceType","\n","[54.1:54.34]"),
                    ("","deriving instance Show DebSource","\n","[55.1:55.33]"),
                    ("\n","sourceSlices :: SliceList -> SliceList","\n","[57.1:57.39]"),
                    ("","sourceSlices = SliceList . filter ((== DebSrc) . sourceType . sliceSource) . slices","\n","[58.1:58.84]"),
                    ("\n","binarySlices :: SliceList -> SliceList","\n","[60.1:60.39]"),
                    ("","binarySlices = SliceList . filter ((== Deb) . sourceType . sliceSource) . slices","\n","[61.1:61.81]"),
                    ("\n","inexactPathSlices :: SliceList -> SliceList","\n","[63.1:63.44]"),
                    ("","inexactPathSlices = SliceList . filter (either (const False) (const True) . sourceDist . sliceSource) . slices","\n","[64.1:64.111]"),
                    ("\n","releaseSlices :: ReleaseName -> SliceList -> SliceList","\n","[66.1:66.55]"),
                    ("","releaseSlices release list =\n    SliceList . filter (isRelease . sourceDist . sliceSource) $ (slices list)\n    where isRelease = either (const False) (\\ (x, _) -> x == release)","\n","[67.1:71.0]"),
                    ("\n","appendSliceLists :: [SliceList] -> SliceList","\n","[71.1:71.45]"),
                    ("","appendSliceLists lists =\n    SliceList { slices = concat (map slices lists) }","\n","[72.1:73.53]"),
                    ("\n{-\n-- |Return the list of releases in a repository, which is the\n-- list of directories in the dists subdirectory.  Currently\n-- this is only known to work with Apache.  Note that some of\n-- the returned directories may be symlinks.\nuriSubdirs :: (Maybe EnvRoot) -> URI -> IO [Text]\nuriSubdirs root uri =\n    liftIO (dirFromURI uri') >>= either throw (return . map pack)\n    where\n      uri' = case uriScheme uri of\n               \"file:\" -> uri {uriPath = maybe \"\" rootPath root ++ (uriPath uri)}\n               _ -> uri\n\nreadRelease :: URI -> Text -> IO (Maybe (Paragraph' Text))\nreadRelease uri name =\n    do output <- liftIO (fileFromURI uri')\n       case output of\n         Left e -> throw e\n         Right s -> case parseControl (show uri') (B.concat . L.toChunks $ s) of\n                      Right (Control [paragraph]) -> return (Just (decodeParagraph paragraph))\n                      _ -> return Nothing\n    where\n      uri' = uri {uriPath = uriPath uri </> \"dists\" </> unpack name </> \"Release\"}\n-}\n\n","data UpdateError\n    = Changed ReleaseName FilePath SliceList SliceList\n    | Missing ReleaseName FilePath\n    | Flushed\n    deriving Typeable","\n","[100.1:104.22]"),
                    ("\n","instance Exception UpdateError","\n","[106.1:106.31]"),
                    ("\n","instance Show UpdateError where\n    show (Changed r p l1 l2) = unwords [\"Changed\", show r, show p, ppDisplay l1, ppDisplay l2]\n    show (Missing r p) = unwords [\"Missing\", show r, show p]\n    show Flushed = \"Flushed\"","\n","[108.1:113.0]"),
                    ("\n","data SourcesChangedAction =\n    SourcesChangedError |\n    UpdateSources |\n    RemoveRelease\n    deriving (Eq, Show, Data, Typeable)","\n","[113.1:117.40]"),
                    ("\n","doSourcesChangedAction :: FilePath -> FilePath -> NamedSliceList -> SliceList -> SourcesChangedAction -> IO ()","\n","[119.1:119.111]"),
                    ("","doSourcesChangedAction dir sources baseSources fileSources SourcesChangedError = do\n  ePutStrLn (\"The sources.list in the existing '\" ++ (relName . sliceListName $ baseSources) ++ \"' in \" ++ dir ++\n             \" apt-get environment doesn't match the parameters passed to the autobuilder\" ++ \":\\n\\n\" ++\n             sources ++ \":\\n\\n\" ++\n             ppDisplay fileSources ++\n\t     \"\\nRun-time parameters:\\n\\n\" ++\n             ppDisplay baseSources ++ \"\\n\" ++\n\t     \"It is likely that the build environment in\\n\" ++\n             dir ++ \" is invalid and should be rebuilt.\")\n  ePutStr $ \"Remove it and continue (or exit)?  [y/n]: \"\n  result <- hGetLine stdin\n  case result of\n    ('y' : _) ->\n        do removeRecursiveSafely dir\n           createDirectoryIfMissing True dir\n           replaceFile sources (ppDisplay baseSources)\n    _ -> error (\"Please remove \" ++ dir ++ \" and restart.\")\n\ndoSourcesChangedAction dir sources baseSources _fileSources RemoveRelease = do\n  ePutStrLn $ \"Removing suspect environment: \" ++ dir\n  removeRecursiveSafely dir\n  createDirectoryIfMissing True dir\n  replaceFile sources (ppDisplay baseSources)\n\ndoSourcesChangedAction dir sources baseSources _fileSources UpdateSources = do\n  -- The sources.list has changed, but it should be\n  -- safe to update it.\n  ePutStrLn $ \"Updating environment with new sources.list: \" ++ dir\n  removeFile sources\n  replaceFile sources (ppDisplay baseSources)","\n","[120.1:150.0]"),
                    ("","","","")]


parseTestOutput :: ModuleInfo -> Seq (String, String, String, String)
parseTestOutput m =
          foldModule tailf pragmaf namef warningf tailf exportf tailf importf declf tailf m mempty
          where
            pragmaf x pref s suff r = r |> (pref, s, suff,int (spanInfo x))
            namef x pref s suff r = r |> (pref, s, suff,int (spanInfo x))
            warningf x pref s suff r = r |> (pref, s, suff,int (spanInfo x))
            exportf x pref s suff r = r |> (pref, s, suff,int (spanInfo x))
            importf x pref s suff r = r |> (pref, s, suff,int (spanInfo x))
            declf x pref s suff r = r |> (pref, s, suff,int (spanInfo x))
            tailf s r = r |> (s, "", "", "")

int :: HasSpanInfo a => a -> String
int x = let (SrcSpanInfo (SrcSpan _ a b c d) _) = spanInfo x in "[" ++ show a ++ "." ++ show b ++ ":" ++ show c ++ "." ++ show d ++ "]"

test3 :: Test
test3 =
    TestLabel "test3" $ TestCase $ withCurrentDirectory "tests/data" $
    do let path = APath "Equal.hs"
       mi <- runModuVerseT $ pathKey path >>= parseModule
       let (output, original) = test mi
       assertEqual "echo" original output
    where
      test :: ModuleInfo -> (String, String)
      test m@(ModuleInfo _ text _ _ _) = (fold (foldModule echo2 echo echo echo echo2 echo echo2 echo echo echo2 m mempty), text)

fold3b :: Test
fold3b =
    TestLabel "fold3b" $ TestCase $ withCurrentDirectory "tests/data" $
    do let path = APath "fold3b/Main.hs"
       mi <- runModuVerseT $ pathKey path >>= parseModule
       let (output, original) = test mi
       assertEqual "echo" original output
    where
      test :: ModuleInfo -> (String, String)
      test m@(ModuleInfo _ text _ _ _) = (fold (foldModule echo2 echo echo echo echo2 echo echo2 echo echo echo2 m mempty), text)

fold3c :: Test
fold3c =
    TestLabel "fold3c" $ TestCase $
    do let path = APath "tests/data/fold9.hs"
       mi <- runModuVerseT $ pathKey path >>= parseModule
       let (output, original) = test mi
       assertEqual "echo" original output
    where
      test :: ModuleInfo -> (String, String)
      test m@(ModuleInfo _ text _ _ _) = (fold (foldModule echo2 echo echo echo echo2 echo echo2 echo echo echo2 m mempty), text)

test5 :: Test
test5 =
    TestLabel "fold5" $ TestCase $
    do let path = APath "tests/data/fold5.hs" -- "tests/data/logic/Data/Logic/Classes/Literal.hs"
       mi <- runModuVerseT $ pathKey path >>= parseModule
       -- let actual = map f (adjustSpans text comments (spans m))
       -- assertEqual "spans" original actual
       let actual = foldDecls (\ _ a b c r -> r ++ [(a, b, c)]) (\ s r -> r ++ [("", s, "")]) mi []
       assertEqual "spans" expected actual
    where
      expected = [("","a = 1 where","\n"),
                  ("\n{- This makes bad things happen. -}\n\n","b = ()","\n"),
                  ("","","")]

test5b :: Test
test5b =
    TestLabel "test5b" $ TestCase $
    do let path = APath "tests/data/logic/Data/Logic/Classes/Literal.hs"
       mi <- runModuVerseT $ pathKey path >>= parseModule
       let actual = foldDecls (\ _ a b c r -> r ++ [(a, b, c)]) (\ s r -> r ++ [("", s, "")]) mi []
       assertEqual "spans" expected actual
    where
      expected = [("\n-- |Literals are the building blocks of the clause and implicative normal\n-- |forms.  They support negation and must include True and False elements.\n",
                   "class (Negatable lit, Constants lit, HasFixity atom, Formula lit atom, Ord lit) => Literal lit atom | lit -> atom where\n    foldLiteral :: (lit -> r) -> (Bool -> r) -> (atom -> r) -> lit -> r",
                   "\n"),
                  ("\n","zipLiterals :: Literal lit atom =>\n               (lit -> lit -> Maybe r)\n            -> (Bool -> Bool -> Maybe r)\n            -> (atom -> atom -> Maybe r)\n            -> lit -> lit -> Maybe r","\n"),
                  ("",
                   "zipLiterals neg tf at fm1 fm2 =\n    foldLiteral neg' tf' at' fm1\n    where\n      neg' p1 = foldLiteral (neg p1) (\\ _ -> Nothing) (\\ _ -> Nothing) fm2\n      tf' x1 = foldLiteral (\\ _ -> Nothing) (tf x1) (\\ _ -> Nothing) fm2\n      at' a1 = foldLiteral (\\ _ -> Nothing) (\\ _ -> Nothing) (at a1) fm2",
                   "\n"),
                  ("\n{- This makes bad things happen.\n-- | We can use an fof type as a lit, but it must not use some constructs.\ninstance FirstOrderFormula fof atom v => Literal fof atom v where\n    foldLiteral neg tf at fm = foldFirstOrder qu co tf at fm\n        where qu = error \"instance Literal FirstOrderFormula\"\n              co ((:~:) x) = neg x\n              co _ = error \"instance Literal FirstOrderFormula\"\n    atomic = Data.Logic.Classes.FirstOrder.atomic\n-}\n\n-- |Just like Logic.FirstOrder.convertFOF except it rejects anything\n-- with a construct unsupported in a normal logic formula,\n-- i.e. quantifiers and formula combinators other than negation.\n",
                   "fromFirstOrder :: forall formula atom v lit atom2.\n                  (Formula lit atom2, FOF.FirstOrderFormula formula atom v, Literal lit atom2) =>\n                  (atom -> atom2) -> formula -> Failing lit",
                   "\n"),
                  ("",
                   "fromFirstOrder ca formula =\n    FOF.foldFirstOrder (\\ _ _ _ -> Failure [\"fromFirstOrder\"]) co (Success . fromBool) (Success . atomic . ca) formula\n    where\n      co :: Combination formula -> Failing lit\n      co ((:~:) f) =  fromFirstOrder ca f >>= return . (.~.)\n      co _ = Failure [\"fromFirstOrder\"]",
                   "\n"),
                  ("\n","fromLiteral :: forall lit atom v fof atom2. (Literal lit atom, FOF.FirstOrderFormula fof atom2 v) =>\n               (atom -> atom2) -> lit -> fof","\n"),
                  ("","fromLiteral ca lit = foldLiteral (\\ p -> (.~.) (fromLiteral ca p)) fromBool (atomic . ca) lit","\n"),
                  ("\n","toPropositional :: forall lit atom pf atom2. (Literal lit atom, P.PropositionalFormula pf atom2) =>\n                   (atom -> atom2) -> lit -> pf","\n"),
                  ("","toPropositional ca lit = foldLiteral (\\ p -> (.~.) (toPropositional ca p)) fromBool (atomic . ca) lit","\n"),
                  ("\n{-\nprettyLit :: forall lit atom term v p f. (Literal lit atom v, Apply atom p term, Term term v f) =>\n              (v -> Doc)\n           -> (p -> Doc)\n           -> (f -> Doc)\n           -> Int\n           -> lit\n           -> Doc\nprettyLit pv pp pf _prec lit =\n    foldLiteral neg tf at lit\n    where\n      neg :: lit -> Doc\n      neg x = if negated x then text {-\"\172\"-} \"~\" <> prettyLit pv pp pf 5 x else prettyLit pv pp pf 5 x\n      tf = text . ifElse \"true\" \"false\"\n      at = foldApply (\\ pr ts -> \n                        pp pr <> case ts of\n                                   [] -> empty\n                                   _ -> parens (hcat (intersperse (text \",\") (map (prettyTerm pv pf) ts))))\n                   (\\ x -> text $ if x then \"true\" else \"false\")\n      -- parensIf False = id\n      -- parensIf _ = parens . nest 1\n-}\n\n","prettyLit :: forall lit atom v. (Literal lit atom) =>\n              (Int -> atom -> Doc)\n           -> (v -> Doc)\n           -> Int\n           -> lit\n           -> Doc","\n"),
                  ("","prettyLit pa pv pprec lit =\n    parensIf (pprec > prec) $ foldLiteral co tf at lit\n    where\n      co :: lit -> Doc\n      co x = if negated x then text {-\"\172\"-} \"~\" <> prettyLit pa pv 5 x else prettyLit pa pv 5 x\n      tf x = text (if x then \"true\" else \"false\")\n      at = pa 6\n      parensIf False = id\n      parensIf _ = parens . nest 1\n      Fixity prec _ = fixityLiteral lit","\n"),
                  ("\n","fixityLiteral :: (Literal formula atom) => formula -> Fixity","\n"),
                  ("","fixityLiteral formula =\n    foldLiteral neg tf at formula\n    where\n      neg _ = Fixity 5 InfixN\n      tf _ = Fixity 10 InfixN\n      at = fixity","\n"),
                  ("\n","foldAtomsLiteral :: Literal lit atom => (r -> atom -> r) -> r -> lit -> r","\n"),
                  ("","foldAtomsLiteral f i lit = foldLiteral (foldAtomsLiteral f i) (const i) (f i) lit","\n"),
                  ("","","")]

test6 :: Test
test6 = TestCase (assertEqual "tree1"
                              (Node {rootLabel = sp 2 20,
                                     subForest = [Node {rootLabel = sp 5 10,
                                                        subForest = [Node {rootLabel = sp 6 7, subForest = []},
                                                                     Node {rootLabel = sp 8 9, subForest = []}]},
                                                  Node {rootLabel = sp 11 18,
                                                        subForest = [Node {rootLabel = sp 12 15, subForest = []}]}]})
                              (makeTree (Set.fromList
                                         [sp 2 20,
                                          sp 5 10,
                                          sp 11 18,
                                          sp 6 7,
                                          sp 8 9,
                                          sp 12 15])))
    where
      sp a b = mkspan (29, a) (29, b)
      mkspan (a, b) (c, d) =
          SrcSpanInfo {srcInfoSpan = SrcSpan {srcSpanFilename = "<unknown>.hs",
                                              srcSpanStartLine = a,
                                              srcSpanStartColumn = b,
                                              srcSpanEndLine = c,
                                              srcSpanEndColumn = d}, srcInfoPoints = []}

test4 :: Test
test4 = TestCase (assertEqual "test4" (SrcLoc "<unknown>.hs" 2 24 < SrcLoc "<unknown>.hs" 29 7) True)

test7 :: Test
test7 =
    TestCase $
    do mi <- runModuVerseT $ pathKey (APath "tests/data/Fold7.hs") >>= parseModule
       let actual = foldModule (\ s r -> r |> (s, "", ""))
                               (\ _ b s a r -> r |> (b, s, a))
                               (\ _ b s a r -> r |> (b, s, a))
                               (\ _ b s a r -> r |> (b, s, a))
                               (\ s r -> r |> (s, "", ""))
                               (\ _ b s a r -> r |> (b, s, a))
                               (\ s r -> r |> (s, "", ""))
                               (\ _ b s a r -> r |> (b, s, a))
                               (\ _ b s a r -> r |> (b, s, a))
                               (\ s r -> r |> (s, "", ""))
                               mi mempty
       assertEqual "fold7" expected actual
    where
      expected = Seq.fromList $
             [ ("module ","","")
             , ("","Main"," ")
             , ("where\n\n-- | Get the contents of a package index\n","","")
            -- What we are getting
             , ("","binaryPackagesOfIndex repo release index =\n    liftIO $ getPackages repo release index"," "), ("-- >>= return . either Left (Right . List.map (toBinaryPackage index . packageInfo))\n\n","a=1","  ")
            -- What we want
             -- , ("","binaryPackagesOfIndex repo release index =\n    liftIO $ getPackages repo release index", " -- >>= return . either Left (Right . List.map (toBinaryPackage index . packageInfo))\n"), ("\n", "a=1","  ")
             , ("-- This is a comment too\n","","") ]
