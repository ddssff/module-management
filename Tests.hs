{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Control.Exception (SomeException, try)
import Data.List (isPrefixOf)
import Data.Set as Set (Set, fromList, union, delete)
import Language.Haskell.Exts.Annotated (defaultParseMode, exactPrint, parseFileWithComments, ParseResult(ParseOk))
import Language.Haskell.Exts.Annotated.Syntax as A (Module)
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Extension (Extension(..))
import Language.Haskell.Exts.Syntax (ModuleName(ModuleName))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Modules (splitModule, mergeModules)
import qualified Tests.Merge as Merge (tests)
import Language.Haskell.Modules.Common (withCurrentDirectory)
import qualified Tests.Fold as Fold (tests)
import qualified Tests.Imports as Imports (tests)
import Language.Haskell.Modules.Internal (MonadClean, Params(extensions, moduVerse), runMonadClean, modifyParams)
import qualified Tests.Split as Split (tests)
import Language.Haskell.Modules.Util.QIO (noisily, qLnPutStr)
import Language.Haskell.Modules.Util.Test (logicModules, diff', rsync)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.Process (system, readProcess)
import Test.HUnit (assertEqual, Counts(..), runTestTT, Test(TestList, TestCase, TestLabel))

main :: IO ()
main =
    do _ <- system "[ -d testdata ] || tar xfz testdata.tar.gz"
       counts <- runTestTT (TestList [TestLabel "Main" Main.tests])
       putStrLn (show counts)
       case (errors counts + failures counts) of
         0 -> exitWith ExitSuccess
         _ -> exitWith (ExitFailure 1)

withTestData :: (A.Module SrcSpanInfo -> [Comment] -> String -> IO r) -> FilePath -> IO r
withTestData f path = withCurrentDirectory "testdata/debian" $
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
                 , TestLabel "Merge" Merge.tests
                 , TestLabel "Fold" Fold.tests
                 , TestLabel "Imports" Imports.tests
                 , TestLabel "Split" Split.tests
                 -- If split-merge-merge fails try split and split-merge.
                 , Main.logictest "split" test2a
                 , Main.logictest "split-merge" test2b
                 , Main.logictest "split-merge-merge" test2c
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
    do _ <- rsync "testdata/logic" "tmp"
       _ <- withCurrentDirectory "tmp" $ runMonadClean $ f logicModules
       (code, out, err) <- diff' ("testdata/" ++ s ++ "-expected") "tmp"
       let out' = unlines (filter (not . isPrefixOf "Binary files") . map (takeWhile (/= '\t')) $ (lines out))
       assertEqual s (ExitSuccess, "", "") (code, out', err)

test2a :: MonadClean m => Set ModuleName -> m ()
test2a u =
         do modifyParams (\ p -> p {extensions = extensions p ++ [MultiParamTypeClasses],
                                    moduVerse = Just u})
            qLnPutStr "Splitting module Literal"
            splitModule (ModuleName "Data.Logic.Classes.Literal")
            return ()

test2b :: MonadClean m => Set ModuleName -> m ()
test2b u =
         do modifyParams (\ p -> p {extensions = extensions p ++ [MultiParamTypeClasses],
                                    moduVerse = Just u})
            qLnPutStr "Splitting module Literal"
            splitModule (ModuleName "Data.Logic.Classes.Literal")
            qLnPutStr "Merging FirstOrder, fromFirstOrder, fromLiteral into FirstOrder"
            -- modifyParams (\ p -> p {testMode = True})
            mergeModules
              [ModuleName "Data.Logic.Classes.FirstOrder",
               ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
               ModuleName "Data.Logic.Classes.Literal.FromLiteral"]
              (ModuleName "Data.Logic.Classes.FirstOrder")
            noisily (qLnPutStr "merge2")
            return ()
    where
      u' = union (fromList [ModuleName "Data.Logic.Classes.Literal.Internal.FixityLiteral",
                            ModuleName "Data.Logic.Classes.Literal.FoldAtomsLiteral",
                            ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
                            ModuleName "Data.Logic.Classes.Literal.FromLiteral",
                            ModuleName "Data.Logic.Classes.Literal.Literal",
                            ModuleName "Data.Logic.Classes.Literal.PrettyLit",
                            ModuleName "Data.Logic.Classes.Literal.ToPropositional",
                            ModuleName "Data.Logic.Classes.Literal.ZipLiterals"])
                 (delete (ModuleName "Data.Logic.Classes.Literal") u)

test2c :: MonadClean m => Set ModuleName -> m ()
test2c u =
         do modifyParams (\ p -> p {extensions = extensions p ++ [MultiParamTypeClasses],
                                    moduVerse = Just u})
            qLnPutStr "Splitting module Literal"
            splitModule (ModuleName "Data.Logic.Classes.Literal")
            qLnPutStr "Merging FirstOrder, fromFirstOrder, fromLiteral into FirstOrder"
            mergeModules
              [ModuleName "Data.Logic.Classes.FirstOrder",
               ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
               ModuleName "Data.Logic.Classes.Literal.FromLiteral"]
              (ModuleName "Data.Logic.Classes.FirstOrder")
            noisily (qLnPutStr "Merging remaining split modules into Literal")
            -- modifyParams (\ p -> p {testMode = True})
            mergeModules
              [ModuleName "Data.Logic.Classes.Literal.Literal",
               ModuleName "Data.Logic.Classes.Literal.ZipLiterals",
               ModuleName "Data.Logic.Classes.Literal.ToPropositional",
               ModuleName "Data.Logic.Classes.Literal.PrettyLit",
               ModuleName "Data.Logic.Classes.Literal.Internal.FixityLiteral",
               ModuleName "Data.Logic.Classes.Literal.FoldAtomsLiteral"]
              (ModuleName "Data.Logic.Classes.Literal")
            return ()
