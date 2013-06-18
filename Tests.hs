{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Control.Exception (SomeException, try)
import Data.List (filter, isPrefixOf)
import Data.Set (fromList, difference)
import Language.Haskell.Exts.Annotated (defaultParseMode, exactPrint, parseFileWithComments, ParseResult(ParseOk))
import Language.Haskell.Exts.Annotated.Syntax as A (Module)
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Modules (withCurrentDirectory, Extension(..), Params(extensions, moduVerse), runCleanT, modifyParams, ModuleName(ModuleName), splitModule, catModules, noisily, qPutStrLn)
import Language.Haskell.Modules.Cat as Cat (tests)
import Language.Haskell.Modules.Fold as Fold (tests)
import Language.Haskell.Modules.Imports as Imports (tests)
import Language.Haskell.Modules.Split as Split (tests)
import System.Cmd (system)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.Process (readProcess, readProcessWithExitCode)
import Test.HUnit (assertEqual, Counts(..), runTestTT, Test(TestList, TestCase, TestLabel))

main :: IO ()
main =
    do counts <- runTestTT (TestList [TestLabel "Cat" Cat.tests,
                                      TestLabel "Fold" Fold.tests,
                                      TestLabel "Imports" Imports.tests,
                                      TestLabel "Main" Main.tests,
                                      TestLabel "Split" Split.tests])
       putStrLn (show counts)
       case (errors counts + failures counts) of
         0 -> exitWith ExitSuccess
         _ -> exitWith (ExitFailure 1)

withTestData :: (A.Module SrcSpanInfo -> [Comment] -> String -> IO r) -> FilePath -> IO r
withTestData f path = withCurrentDirectory "testdata/original" $
    do text <- try (readFile path)
       source <- try (parseFileWithComments defaultParseMode path)
       case (text, source) of
         (Right text', Right (ParseOk (m, comments))) ->
             f m comments text'
         (Right _, Right _) -> error "parse failure"
         (Left (e :: SomeException), _) -> error $ "failure: " ++ show e
         (_, Left (e :: SomeException)) -> error $ "failure: " ++ show e

tests :: Test
tests = TestList [Main.test1, Main.test2]

test1 :: Test
test1 =
    TestLabel "exactPrint" $ TestCase
    (withTestData test "Debian/Repo/Package.hs" >>= \ (output, text) ->
     assertEqual
     "exactPrint"
     text
     output)
    where
      test parsed comments text = return (exactPrint parsed comments, text)

test2 :: Test
test2 =
    TestLabel "split-cat-cat" $ TestCase $
    do _ <- readProcess "rsync" ["-aHxS", "--delete", {-"-v",-} "testdata/logic/", "testdata/copy"] ""
       _ <- withCurrentDirectory "testdata/copy" $ runCleanT $
         do modifyParams (\ p -> p {extensions = extensions p ++ [MultiParamTypeClasses],
                                    moduVerse = Just u})
            noisily (qPutStrLn "split")
            splitModule (ModuleName "Data.Logic.Classes.Literal")
            noisily (qPutStrLn "cat1")
            catModules
              u
              [ModuleName "Data.Logic.Classes.FirstOrder",
               ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
               ModuleName "Data.Logic.Classes.Literal.FromLiteral"]
              (ModuleName "Data.Logic.Classes.FirstOrder")
            noisily (qPutStrLn "cat2")
            catModules
              u'
              [ModuleName "Data.Logic.Classes.Literal.FixityLiteral",
               ModuleName "Data.Logic.Classes.Literal.FoldAtomsLiteral",
               ModuleName "Data.Logic.Classes.Literal.Literal",
               ModuleName "Data.Logic.Classes.Literal.PrettyLit",
               ModuleName "Data.Logic.Classes.Literal.ToPropositional",
               ModuleName "Data.Logic.Classes.Literal.ZipLiterals"]
              (ModuleName "Data.Logic.Classes.Literal")
       (code, out, err) <- readProcessWithExitCode "diff" ["-ru", "--unidirectional-new-file", "--exclude=*~", "--exclude=*.imports", "testdata/split-cat-cat-result", "testdata/copy"] ""
       let out' = unlines (filter (not . isPrefixOf "Binary files") . map (takeWhile (/= '\t')) $ (lines out))
       assertEqual "split-cat-cat" (ExitSuccess, "", "") (code, out', err)
    where
      u' = difference u (fromList [ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
                                       ModuleName "Data.Logic.Classes.Literal.FromLiteral"])
      u = fromList
            (map ModuleName 
                      ["Data.Boolean.SatSolver",
                       "Data.Boolean",
                       "Data.Logic.Resolution",
                       "Data.Logic.KnowledgeBase",
                       "Data.Logic.Types.FirstOrder",
                       "Data.Logic.Types.Common",
                       "Data.Logic.Types.Harrison.Formulas.FirstOrder",
                       "Data.Logic.Types.Harrison.Formulas.Propositional",
                       "Data.Logic.Types.Harrison.Prop",
                       "Data.Logic.Types.Harrison.Equal",
                       "Data.Logic.Types.Harrison.FOL",
                       "Data.Logic.Types.Propositional",
                       "Data.Logic.Types.FirstOrderPublic",
                       "Data.Logic.Harrison.Unif",
                       "Data.Logic.Harrison.Meson",
                       "Data.Logic.Harrison.Herbrand",
                       "Data.Logic.Harrison.Formulas.FirstOrder",
                       "Data.Logic.Harrison.Formulas.Propositional",
                       "Data.Logic.Harrison.Tests",
                       "Data.Logic.Harrison.Resolution",
                       "Data.Logic.Harrison.DefCNF",
                       "Data.Logic.Harrison.Skolem",
                       "Data.Logic.Harrison.Prop",
                       "Data.Logic.Harrison.DP",
                       "Data.Logic.Harrison.Lib",
                       "Data.Logic.Harrison.PropExamples",
                       "Data.Logic.Harrison.Prolog",
                       "Data.Logic.Harrison.Tableaux",
                       "Data.Logic.Harrison.Equal",
                       "Data.Logic.Harrison.Normal",
                       "Data.Logic.Harrison.FOL",
                       "Data.Logic.Tests.TPTP",
                       "Data.Logic.Tests.Common",
                       "Data.Logic.Tests.Harrison.Unif",
                       "Data.Logic.Tests.Harrison.Meson",
                       "Data.Logic.Tests.Harrison.Resolution",
                       "Data.Logic.Tests.Harrison.Common",
                       "Data.Logic.Tests.Harrison.Skolem",
                       "Data.Logic.Tests.Harrison.Prop",
                       "Data.Logic.Tests.Harrison.Main",
                       "Data.Logic.Tests.Harrison.Equal",
                       "Data.Logic.Tests.Harrison.FOL",
                       "Data.Logic.Tests.Main",
                       "Data.Logic.Tests.Logic",
                       "Data.Logic.Tests.Data",
                       "Data.Logic.Tests.HUnit",
                       "Data.Logic.Tests.Chiou0",
                       "Data.Logic.Failing",
                       "Data.Logic.Instances.SatSolver",
                       "Data.Logic.Instances.TPTP",
                       "Data.Logic.Instances.Chiou",
                       "Data.Logic.Instances.PropLogic",
                       "Data.Logic.Normal.Clause",
                       "Data.Logic.Normal.Implicative",
                       "Data.Logic.Classes.FirstOrder",
                       "Data.Logic.Classes.Variable",
                       "Data.Logic.Classes.Apply",
                       "Data.Logic.Classes.Negate",
                       "Data.Logic.Classes.Pretty",
                       "Data.Logic.Classes.Arity",
                       "Data.Logic.Classes.Skolem",
                       "Data.Logic.Classes.Combine",
                       "Data.Logic.Classes.Constants",
                       "Data.Logic.Classes.Equals",
                       "Data.Logic.Classes.Propositional",
                       "Data.Logic.Classes.Atom",
                       "Data.Logic.Classes.Formula",
                       "Data.Logic.Classes.ClauseNormalForm",
                       "Data.Logic.Classes.Term",
                       "Data.Logic.Classes.Literal",
                       "Data.Logic.Classes.Literal.FoldAtomsLiteral",
                       "Data.Logic.Classes.Literal.ToPropositional",
                       "Data.Logic.Classes.Literal.FromFirstOrder",
                       "Data.Logic.Classes.Literal.FixityLiteral",
                       "Data.Logic.Classes.Literal.FromLiteral",
                       "Data.Logic.Classes.Literal.PrettyLit",
                       "Data.Logic.Classes.Literal.ZipLiterals",
                       "Data.Logic.Classes.Literal.Literal",
                       "Data.Logic.Satisfiable"])
