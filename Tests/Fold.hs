module Tests.Fold where

import Data.Foldable (fold)
import Data.Monoid (Monoid(mempty))
import Data.Sequence as Seq (filter, fromList, Seq, zip, (|>))
import Data.Set.Extra as Set (fromList)
import Data.Tree (Tree(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldModule)
import Language.Haskell.Modules.Internal (runCleanT)
import Language.Haskell.Modules.ModuVerse (ModuleInfo(..), parseModule)
import Language.Haskell.Modules.SourceDirs (pathKey)
import Language.Haskell.Modules.Util.SrcLoc (HasSpanInfo(..), makeTree)
import Test.HUnit (assertEqual, Test(TestList, TestCase, TestLabel))

tests :: Test
tests = TestLabel "Clean" (TestList [test1, test1b, test3, test4, test5, test5b, test6, test7])

test1 :: Test
test1 =
    TestLabel "test1" $ TestCase $ withCurrentDirectory "testdata/debian" $
    do let path = "Debian/Repo/Orphans.hs"
       mi <- runCleanT $ pathKey path >>= parseModule
       let (output, original) = test mi
       assertEqual "echo" original output
    where
      test :: ModuleInfo -> (String, String)
      test m@(ModuleInfo _ text _ _) = (fold (foldModule echo2 echo echo echo echo2 echo echo2 echo echo echo2 m mempty), text)

test1b :: Test
test1b =
    TestLabel "test1b" $ TestCase $ withCurrentDirectory "testdata/debian" $
    do let path = "Debian/Repo/Sync.hs"
       mi <- runCleanT $ pathKey path >>= parseModule
       let output = test mi
       assertEqual "echo" mempty (Seq.filter (\ (a, b) -> a /= b) (Seq.zip expected output))
    where
      expected :: Seq (String, String, String, String)
      expected = Seq.fromList
                   [("-- Comment above module head\nmodule ","","",""),
                    ("","Debian.Repo.Sync"," ","[2.8:2.24]"),
                    ("","{-# WARNING \"this is a warning\" #-}","\n","[2.25:2.60]"),
                    ("    ( ","","",""),
                    ("","rsync","\n","[3.7:3.12]"),
                    ("    , ","foo","\n","[4.7:4.10]"),
                    ("    -- Comment after last export\n    ) where","","",""),
                    ("\n\n-- Comment before first import\n\n","import Control.Monad.Trans (MonadIO)","\n","[10.1:10.37]"),
                    ("","import qualified Data.ByteString as B (empty)","\n","[11.1:11.46]"),
                    ("","import System.Exit (ExitCode)","\n","[12.1:12.30]"),
                    ("","import System.FilePath (dropTrailingPathSeparator)","\n","[13.1:13.51]"),
                    ("-- Comment between two imporrts\n","import System.Process (proc)","\n","[15.1:15.29]"),
                    ("","import System.Process.Progress (keepResult, runProcessF)","\n","[16.1:16.57]"),
                    ("\n-- Comment before first decl\n","rsync :: (Functor m, MonadIO m) => [String] -> FilePath -> FilePath -> m ExitCode","\n","[19.1:19.82]"),
                    ("","rsync extra source dest =\n    do result <- runProcessF (proc \"rsync\" ([\"-aHxSpDt\", \"--delete\"] ++ extra ++\n                                            [dropTrailingPathSeparator source ++ \"/\",\n                                             dropTrailingPathSeparator dest])) B.empty >>= return . keepResult\n       case result of\n         [x] -> return x\n         _ -> error \"Missing or multiple exit codes\"","\n","[20.1:29.0]"),
                    ("\n-- Comment between two decls\n","foo :: Int","\n","[29.1:29.11]"),
                    ("","foo = 1","\n","[30.1:30.8]"),
                    ("\n{-\nhandleExit 1 = \"Syntax or usage error\"\nhandleExit 2 = \"Protocol incompatibility\"\nhandleExit 3 = \"Errors selecting input/output files, dirs\"\nhandleExit 4 = \"Requested action not supported: an attempt was made to manipulate 64-bit files on a platform that cannot support them; or an option was specified that is supported by the client and not by the server.\"\nhandleExit 5 = \"Error starting client-server protocol\"\nhandleExit 6 = \"Daemon unable to append to log-file\"\nhandleExit 10 = \"Error in socket I/O\"\nhandleExit 11 = \"Error in file I/O\"\nhandleExit 12 = \"Error in rsync protocol data stream\"\nhandleExit 13 = \"Errors with program diagnostics\"\nhandleExit 14 = \"Error in IPC code\"\nhandleExit 20 = \"Received SIGUSR1 or SIGINT\"\nhandleExit 21 = \"Some error returned by waitpid()\"\nhandleExit 22 = \"Error allocating core memory buffers\"\nhandleExit 23 = \"Partial transfer due to error\"\nhandleExit 24 = \"Partial transfer due to vanished source files\"\nhandleExit 25 = \"The --max-delete limit stopped deletions\"\nhandleExit 30 = \"Timeout in data send/receive\"\nhandleExit 35 = \"Timeout waiting for daemon connection\"\n-}\n","","","")]
      test :: ModuleInfo -> Seq (String, String, String, String)
      test m =
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
    TestLabel "test3" $ TestCase $ withCurrentDirectory "testdata" $
    do let path = "Equal.hs"
       mi <- runCleanT $ pathKey path >>= parseModule
       let (output, original) = test mi
       assertEqual "echo" original output
    where
      test :: ModuleInfo -> (String, String)
      test m@(ModuleInfo _ text _ _) = (fold (foldModule echo2 echo echo echo echo2 echo echo2 echo echo echo2 m mempty), text)

test5 :: Test
test5 =
    TestLabel "fold5" $ TestCase $
    do let path = "testdata/fold5.hs" -- "testdata/logic/Data/Logic/Classes/Literal.hs"
       mi <- runCleanT $ pathKey path >>= parseModule
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
    do let path = "testdata/logic/Data/Logic/Classes/Literal.hs"
       mi <- runCleanT $ pathKey path >>= parseModule
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
    do mi <- runCleanT $ pathKey "testdata/Fold7.hs" >>= parseModule
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
