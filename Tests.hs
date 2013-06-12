{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Control.Exception (SomeException, try)
import Language.Haskell.Exts.Annotated (defaultParseMode, exactPrint, parseFileWithComments, ParseResult(ParseOk))
import Language.Haskell.Exts.Annotated.Syntax as A (Module)
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Modules.Cat as Cat (test1, test2)
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Fold as Fold (test1)
import Language.Haskell.Modules.Imports as Imports (tests)
import Language.Haskell.Modules.Split as Split (tests)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import Test.HUnit (assertEqual, Counts(..), runTestTT, Test(TestList, TestCase, TestLabel))

main :: IO ()
main =
    do counts <- runTestTT (TestList [TestLabel "Cat1" Cat.test1,
                                      TestLabel "Cat2" Cat.test2,
                                      TestLabel "Fold" Fold.test1,
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
tests = TestList [Main.test1 {-, test2-}]

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
