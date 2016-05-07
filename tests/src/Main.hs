{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Control.Exception (SomeException, try)
import Language.Haskell.Exts.Annotated (defaultParseMode, {-exactPrint,-} parseFileWithComments, ParseResult(ParseOk))
import Language.Haskell.Exts.Annotated.Syntax as A (Module)
import Language.Haskell.Exts.Comments (Comment)
--import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
--import Language.Haskell.Exts.Syntax (ModuleName(ModuleName))
import Language.Haskell.Modules ({-mergeModules, extensions, ModuVerse, CleanMode(DoClean), noisily, putModule, runModuVerseT,-} withCurrentDirectory)
--import Language.Haskell.Modules.Util.Test (diff', logicModules, rsync)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
--import System.Process (system)
import Test.HUnit ({-assertEqual,-} Counts(..), runTestTT, Test(TestList, {-TestCase,-} TestLabel))

import qualified Fold as Fold (tests)
import qualified Imports as Imports (tests)
import qualified Merge as Merge (tests)
import qualified Split as Split (tests)
import qualified SrcLoc as SrcLoc (tests)
import qualified Symbols as Symbols (tests)

main :: IO ()
main =
    do -- _ <- system "[ -d testdata ] || tar xfz testdata.tar.gz"
       counts <- runTestTT Main.tests
       putStrLn (show counts)
       case (errors counts + failures counts) of
         0 -> exitWith ExitSuccess
         _ -> exitWith (ExitFailure 1)

withTestData :: (A.Module SrcSpanInfo -> [Comment] -> String -> IO r) -> FilePath -> IO r
withTestData f path = withCurrentDirectory "tests/data/debian" $
    do text <- try (readFile path)
       source <- try (parseFileWithComments defaultParseMode path)
       case (text, source) of
         (Right text', Right (ParseOk (m, comments))) ->
             f m comments text'
         (Right _, Right _) -> error "parse failure"
         (Left (e :: SomeException), _) -> error $ "failure: " ++ show e
         (_, Left (e :: SomeException)) -> error $ "failure: " ++ show e

tests :: Test
tests = TestList [
                   TestLabel "Symbols" Symbols.tests
                 , TestLabel "SrcLoc" SrcLoc.tests
                 , TestLabel "Fold" Fold.tests
                 , TestLabel "Imports" Imports.tests
                 , TestLabel "Split" Split.tests
                 , TestLabel "Merge" Merge.tests
                 -- , Main.test1
                 ]

{-
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

slow :: Test
slow = TestList [ -- No need to do test2b or test2a if test2c passes.
                  Main.logictest "split-merge-merge" test2c
                  -- , Main.logictest "split-merge" test2b
                  -- , Main.logictest "split" test2a
                , Split.slow
                ]

-- logictest :: String -> ([String] -> StateT Params m ()) -> Test
logictest :: String -> ([ModuleName] -> StateT Params IO ()) -> Test
logictest s f =
    TestLabel s $ TestCase $
    do _ <- rsync "tests/data/logic" "tmp"
       _ <- withCurrentDirectory "tmp" $ runModuVerseT $ noisily $ f (map ModuleName logicModules)
       (code, out, err) <- diff' ("tests/data/" ++ s ++ "-expected") "tmp"
       let out' = unlines (filter (not . isPrefixOf "Binary files") . map (takeWhile (/= '\t')) $ (lines out))
       assertEqual s (ExitSuccess, "", "") (code, out', err)

test2a :: ModuVerse m => [ModuleName] -> m ()
test2a u =
         do extensions %= (++ [EnableExtension MultiParamTypeClasses])
            -- We *must* clean the split results, or there will be
            -- circular imports created when we merge.
            mapM_ putModule u
            _ <- splitModuleDecls DoClean "Data/Logic/Classes/Literal.hs"
            return ()

test2b :: ModuVerse m => [ModuleName] -> m ()
test2b u =
         do extensions %= (++ [EnableExtension MultiParamTypeClasses])
            mapM_ putModule u
            _ <- splitModuleDecls DoClean "Data/Logic/Classes/Literal.hs"
            _ <- mergeModules
                   DoClean
                   [ModuleName "Data.Logic.Classes.FirstOrder",
                    ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
                    ModuleName "Data.Logic.Classes.Literal.FromLiteral"]
                   (ModuleName "Data.Logic.Classes.FirstOrder")
            return ()

test2c :: ModuVerse m => [ModuleName] -> m ()
test2c u =
         do extensions %= (++ [EnableExtension MultiParamTypeClasses])
            mapM_ putModule u
            _ <- splitModuleDecls DoClean "Data/Logic/Classes/Literal.hs"
            _ <- mergeModules
                   DoClean
                   [ModuleName "Data.Logic.Classes.FirstOrder",
                    ModuleName "Data.Logic.Classes.Literal.FromFirstOrder",
                    ModuleName "Data.Logic.Classes.Literal.FromLiteral"]
                   (ModuleName "Data.Logic.Classes.FirstOrder")
            _ <- mergeModules
                   DoClean
                   [ModuleName "Data.Logic.Classes.Literal.Literal",
                    ModuleName "Data.Logic.Classes.Literal.ZipLiterals",
                    ModuleName "Data.Logic.Classes.Literal.ToPropositional",
                    ModuleName "Data.Logic.Classes.Literal.PrettyLit",
                    ModuleName "Data.Logic.Classes.Literal.Internal.FixityLiteral",
                    ModuleName "Data.Logic.Classes.Literal.FoldAtomsLiteral"]
                   (ModuleName "Data.Logic.Classes.Literal")
            return ()
-}
