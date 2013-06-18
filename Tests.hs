{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Control.Exception (SomeException, try)
import Data.List (filter, isPrefixOf)
import Data.Set (Set, fromList, difference, union)
import Language.Haskell.Exts.Annotated (defaultParseMode, exactPrint, parseFileWithComments, ParseResult(ParseOk))
import Language.Haskell.Exts.Annotated.Syntax as A (Module)
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Modules (withCurrentDirectory, MonadClean, Extension(..), Params(extensions, moduVerse, testMode), runCleanT, modifyParams, ModuleName(ModuleName), splitModule, catModules, noisily, qPutStrLn)
import qualified Language.Haskell.Modules.Cat as Cat (tests)
import qualified Language.Haskell.Modules.Fold as Fold (tests)
import qualified Language.Haskell.Modules.Imports as Imports (tests)
import qualified Language.Haskell.Modules.Split as Split (tests)
import System.Cmd (system)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.Process (readProcess, readProcessWithExitCode)
import Test.HUnit (assertEqual, Counts(..), runTestTT, Test(TestList, TestCase, TestLabel))

main :: IO ()
main =
    do counts <- runTestTT (TestList [TestLabel "Main" Main.tests])
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
tests = TestList [ Main.test1
                 , Main.logictest "split" test2a
                 -- , Main.logictest "split-cat" test2b
                 -- , Main.logictest "split-cat-cat" test2c
                 , TestLabel "Cat" Cat.tests
                 , TestLabel "Fold" Fold.tests
                 , TestLabel "Imports" Imports.tests
                 , TestLabel "Split" Split.tests
                 ]

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

-- logictest :: MonadClean m => String -> (Set ModuleName -> m ()) -> Test
logictest s f =
    TestLabel s $ TestCase $
    do _ <- readProcess "rsync" ["-aHxS", "--delete", {-"-v",-} "testdata/logic/", "testdata/copy"] ""
       _ <- withCurrentDirectory "testdata/copy" $ runCleanT $ f u
       (code, out, err) <- readProcessWithExitCode "diff" ["-ru", "--unidirectional-new-file", "--exclude=*~", "--exclude=*.imports", "testdata/" ++ s ++ "-result", "testdata/copy"] ""
       let out' = unlines (filter (not . isPrefixOf "Binary files") . map (takeWhile (/= '\t')) $ (lines out))
       assertEqual s (ExitSuccess, "", "") (code, out', err)
    where
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
                       "Data.Logic.Satisfiable"])

test2a :: MonadClean m => Set ModuleName -> m ()
test2a u =
         do modifyParams (\ p -> p {extensions = extensions p ++ [MultiParamTypeClasses],
                                    moduVerse = Just u})
            noisily (qPutStrLn "split")
            splitModule (ModuleName "Data.Logic.Classes.Literal")
            return ()

test2b :: MonadClean m => Set ModuleName -> m ()
test2b u =
         do modifyParams (\ p -> p {extensions = extensions p ++ [MultiParamTypeClasses],
                                    moduVerse = Just u,
                                    testMode = True})
            noisily (qPutStrLn "split")
            splitModule (ModuleName "Data.Logic.Classes.Literal")
            noisily (qPutStrLn "cat1")
            catModules
              u'
              [ModuleName "Data.Logic.Classes.FirstOrder",
               ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
               ModuleName "Data.Logic.Classes.Literal.FromLiteral"]
              (ModuleName "Data.Logic.Classes.FirstOrder")
            noisily (qPutStrLn "cat2")
            return ()
    where
      u' = union (fromList [ModuleName "Data.Logic.Classes.Literal.FixityLiteral",
                            ModuleName "Data.Logic.Classes.Literal.FoldAtomsLiteral",
                            ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
                            ModuleName "Data.Logic.Classes.Literal.FromLiteral",
                            ModuleName "Data.Logic.Classes.Literal.Literal",
                            ModuleName "Data.Logic.Classes.Literal.PrettyLit",
                            ModuleName "Data.Logic.Classes.Literal.ToPropositional",
                            ModuleName "Data.Logic.Classes.Literal.ZipLiterals"]) u

test2c :: MonadClean m => Set ModuleName -> m ()
test2c u =
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
            return ()
    where
      u' = difference u (fromList [ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
                                       ModuleName "Data.Logic.Classes.Literal.FromLiteral"])
