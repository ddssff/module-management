{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Control.Exception (SomeException, try)
import Control.Monad.State (StateT)
import Data.List (isPrefixOf)
import Data.Set.Extra as Set (Set, mapM_)
import Language.Haskell.Exts.Annotated (defaultParseMode, exactPrint, parseFileWithComments, ParseResult(ParseOk))
import Language.Haskell.Exts.Annotated.Syntax as A (Module)
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Extension (Extension(..))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax (ModuleName(ModuleName))
import Language.Haskell.Modules (mergeModules, splitModuleDecls)
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Internal (Params, MonadClean, runMonadClean)
import Language.Haskell.Modules.ModuVerse (putName, modifyExtensions)
import Language.Haskell.Modules.Util.QIO (noisily, qLnPutStr)
import Language.Haskell.Modules.Util.Test (diff', logicModules, rsync)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.Process (system)
import Test.HUnit (assertEqual, Counts(..), runTestTT, Test(TestList, TestCase, TestLabel))
import qualified Tests.Fold as Fold (tests)
import qualified Tests.Imports as Imports (tests)
import qualified Tests.Merge as Merge (tests)
import qualified Tests.Split as Split (tests)

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

logictest :: String -> (Set ModuleName -> StateT Params IO ()) -> Test
logictest s f =
    TestLabel s $ TestCase $
    do _ <- rsync "testdata/logic" "tmp"
       _ <- withCurrentDirectory "tmp" $ runMonadClean $ f logicModules
       (code, out, err) <- diff' ("testdata/" ++ s ++ "-expected") "tmp"
       let out' = unlines (filter (not . isPrefixOf "Binary files") . map (takeWhile (/= '\t')) $ (lines out))
       assertEqual s (ExitSuccess, "", "") (code, out', err)

test2a :: MonadClean m => Set ModuleName -> m ()
test2a u =
         do modifyExtensions (++ [MultiParamTypeClasses])
            Set.mapM_ putName u
            qLnPutStr "Splitting module Literal"
            splitModuleDecls "Data/Logic/Classes/Literal.hs"
            return ()

test2b :: MonadClean m => Set ModuleName -> m ()
test2b u =
         do modifyExtensions (++ [MultiParamTypeClasses])
            Set.mapM_ putName u
            qLnPutStr "Splitting module Literal"
            splitModuleDecls "Data/Logic/Classes/Literal.hs"
            qLnPutStr "Merging FirstOrder, fromFirstOrder, fromLiteral into FirstOrder"
            -- modifyParams (\ p -> p {testMode = True})
            _ <- mergeModules
                   [ModuleName "Data.Logic.Classes.FirstOrder",
                    ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
                    ModuleName "Data.Logic.Classes.Literal.FromLiteral"]
                   (ModuleName "Data.Logic.Classes.FirstOrder")
            noisily (qLnPutStr "merge2")
            return ()

test2c :: MonadClean m => Set ModuleName -> m ()
test2c u =
         do modifyExtensions (++ [MultiParamTypeClasses])
            Set.mapM_ putName u
            qLnPutStr "Splitting module Literal"
            splitModuleDecls "Data/Logic/Classes/Literal.hs"
            qLnPutStr "Merging FirstOrder, fromFirstOrder, fromLiteral into FirstOrder"
            _ <- mergeModules
                   [ModuleName "Data.Logic.Classes.FirstOrder",
                    ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
                    ModuleName "Data.Logic.Classes.Literal.FromLiteral"]
                   (ModuleName "Data.Logic.Classes.FirstOrder")
            noisily (qLnPutStr "Merging remaining split modules into Literal")
            -- modifyParams (\ p -> p {testMode = True})
            _ <- mergeModules
                   [ModuleName "Data.Logic.Classes.Literal.Literal",
                    ModuleName "Data.Logic.Classes.Literal.ZipLiterals",
                    ModuleName "Data.Logic.Classes.Literal.ToPropositional",
                    ModuleName "Data.Logic.Classes.Literal.PrettyLit",
                    ModuleName "Data.Logic.Classes.Literal.Internal.FixityLiteral",
                    ModuleName "Data.Logic.Classes.Literal.FoldAtomsLiteral"]
                   (ModuleName "Data.Logic.Classes.Literal")
            return ()
