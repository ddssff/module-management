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
import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldModule, ModuleInfo(..))
import Language.Haskell.Modules.ModuVerse (parseModule, runModuVerseT)
import Language.Haskell.Modules.SourceDirs (pathKey, APath(..))
import Language.Haskell.Modules.SrcLoc (HasSpanInfo(..), makeTree)
import Language.Haskell.Modules.Symbols (foldDeclared)
import Test.HUnit (assertEqual, Test(TestList, TestCase, TestLabel))

deriving instance Eq (Exts.ParseResult (A.Module SrcSpanInfo, [A.Comment]))
deriving instance Show (Exts.ParseMode)

tests :: Test
tests = TestLabel "Clean" (TestList [{-test1, test1b,-} test3a, fold3b, fold3c, test4, test5, {-test5b,-} test6, test7])

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

test3a :: Test
test3a =
    TestLabel "test3a" $ TestCase $ withCurrentDirectory "tests/data" $
    do let path = APath "imports7/CLI.hs"
       mi <- runModuVerseT $ pathKey path >>= parseModule
       let syms = test mi
       assertEqual "Equal.hs symbols" expected syms
    where
      test :: ModuleInfo -> [S.Name]
      test m@(ModuleInfo (A.Module _ _ _ _ ds) text _ _ _) = foldr (\d r -> foldDeclared (\x r' -> x : r') r d) [] ds
      expected = [S.Ident "HMM",
                  S.Ident "defaultHMM",
                  S.Ident "enumHelp",
                  S.Ident "rangeOfBounded",
                  S.Ident "toEnumBounded",
                  S.Ident "toEnumBounded",
                  S.Ident "main",
                  S.Ident "main",
                  S.Ident "noisily'",
                  S.Ident "quietly'",
                  S.Ident "quietly'",
                  S.Ident "noisily'",
                  S.Ident "compl",
                  S.Ident "compl",
                  S.Ident "compl",
                  S.Ident "moduleNameToStr",
                  S.Ident "moduleNameToStr",
                  S.Ident "takesModuleNames",
                  S.Ident "takesModuleNames",
                  S.Ident "cli",
                  S.Ident "cli",
                  S.Ident "CmdM",
                  S.Ident "askConf",
                  S.Ident "askConf",
                  S.Ident "liftCT",
                  S.Ident "liftCT",
                  S.Ident "liftS",
                  S.Ident "liftS",
                  S.Ident "cmd",
                  S.Ident "cmd",
                  S.Ident "cmd",
                  S.Ident "Callback",
                  S.Ident "Command",
                  S.Ident "commandNames",
                  S.Ident "commandNames",
                  S.Ident "cmds_",
                  S.Ident "cmds_",
                  S.Ident "helpMessage",
                  S.Ident "helpMessage",
                  S.Ident "cabalPrint",
                  S.Ident "cabalPrint",
                  S.Ident "cabalRead",
                  S.Ident "cabalWrite",
                  S.Ident "cabalWrite",
                  S.Ident "cabalWrite",
                  S.Ident "cabalWrite",
                  S.Ident "cabalRead",
                  S.Ident "cabalRead",
                  S.Ident "unModuleName",
                  S.Ident "unModuleName",
                  S.Ident "verse",
                  S.Ident "verse",
                  S.Ident "verse",
                  S.Ident "showVerse",
                  S.Ident "showVerse",
                  S.Ident "dir",
                  S.Ident "dir",
                  S.Ident "dir",
                  S.Ident "clean",
                  S.Ident "clean",
                  S.Ident "clean",
                  S.Ident "splitBy",
                  S.Ident "splitBy",
                  S.Ident "splitBy",
                  S.Ident "merge",
                  S.Ident "merge"]

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
