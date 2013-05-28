{-# LANGUAGE CPP, ScopedTypeVariables #-}

import Control.Exception (SomeException, try)
import Data.Default (def)
import Data.Set (Set)
import Data.Set.Extra (gFind)
import Language.Haskell.Exts.Annotated
-- import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(..))
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Annotated.ExactPrint (exactPrint)
import Language.Haskell.Exts.Annotated.Syntax as Syntax
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import Language.Haskell.Imports.Clean as Clean (test1)
import Language.Haskell.Imports.Common (untabify, withCurrentDirectory)
import Language.Haskell.Imports.Fold as Fold (tests)
import Language.Haskell.Imports.FoldAnnotated as FoldAnnotated (tests)
-- import Language.Haskell.Imports.Move as Move (test2)
import Language.Haskell.Imports.Split as Split (tests)
import Language.Haskell.Imports.SrcLoc as SrcLoc (tests)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import Test.HUnit (runTestTT, Test(TestList, TestCase, TestLabel), assertEqual, Counts(..))

main =
    do counts <- runTestTT (TestList [TestLabel "Clean" Clean.test1,
                                      TestLabel "Fold" Fold.tests,
                                      TestLabel "FoldAnnotated" FoldAnnotated.tests,
                                      TestLabel "Move" Move.test2,
                                      TestLabel "Main" Main.tests,
                                      TestLabel "SrcLoc" SrcLoc.tests,
                                      TestLabel "Split" Split.tests])
       putStrLn (show counts)
       case (errors counts + failures counts) of
         0 -> exitWith ExitSuccess
         _ -> exitWith (ExitFailure 1)

-- withTestData :: (Module -> [Comment] -> String -> IO r) -> FilePath -> IO r
withTestData f path = withCurrentDirectory "testdata/original" $
    do text <- try (readFile path)
       source <- try (parseFileWithComments defaultParseMode path)
       case (text, source) of
         (Right text', Right (ParseOk (m, comments))) ->
             f m comments (untabify text')
         (Right _, Right _) -> error "parse failure"
         (Left (e :: SomeException), _) -> error $ "failure: " ++ show e
         (_, Left (e :: SomeException)) -> error $ "failure: " ++ show e

-- withTestModule :: (Module -> IO r) -> FilePath -> IO r
withTestModule f path = withTestData (\ m _ _ -> f m) path

{-
-- foo :: IO (Set Name)
foo = withTestModule (return . gFind) "Debian/Repo/Package.hs"

-- bar :: IO (Set QName)
bar = withTestModule (return . gFind) "Debian/Repo/Package.hs"
-}

tests = TestList [Main.test1 {-, test2-}]

test1 =
    TestLabel "exactPrint" $ TestCase
    (withTestData test "Debian/Repo/Package.hs" >>= \ (output, text) ->
     assertEqual
     "exactPrint"
     text
     output)
    where
      test parsed comments text = return (exactPrint parsed comments, text)

{-
test2 =
    TestLabel "Annoated Parse" $ TestCase
    (withTestData test "Debian/Repo/Package.hs" >>= \ parsed ->
     assertEqual
     "exactPrint"
     (Syntax.Module (SrcSpanInfo {srcInfoSpan = SrcSpan {srcSpanFilename = "<unkonwn>.hs",
                                                         srcSpanStartLine = 1,
                                                         srcSpanStartColumn = 1,
                                                         srcSpanEndLine = 1,
                                                         srcSpanEndColumn = 1},
                                  srcInfoPoints = []}) Nothing [] [] [])
     parsed)
    where
      test parsed comments text = return parsed
-}

modify parsed comments =
    ImportDecl {importAnn = SrcSpanInfo {srcInfoSpan = SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 55, srcSpanStartColumn = 1, srcSpanEndLine = 55, srcSpanEndColumn = 45},
                                         srcInfoPoints = [SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 55, srcSpanStartColumn = 1, srcSpanEndLine = 55, srcSpanEndColumn = 7}]},
                importModule = ModuleName (SrcSpanInfo {srcInfoSpan = SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 55, srcSpanStartColumn = 8, srcSpanEndLine = 55, srcSpanEndColumn = 24}, srcInfoPoints = []}) "System.IO.Unsafe",
                importQualified = False,
                importSrc = False,
                importPkg = Nothing,
                importAs = Nothing,
                importSpecs = Just (ImportSpecList (SrcSpanInfo {srcInfoSpan = SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 55, srcSpanStartColumn = 25, srcSpanEndLine = 55, srcSpanEndColumn = 45},
                                                                 srcInfoPoints = [SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 55, srcSpanStartColumn = 25, srcSpanEndLine = 55, srcSpanEndColumn = 26},
                                                                                  SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 55, srcSpanStartColumn = 44, srcSpanEndLine = 55, srcSpanEndColumn = 45}]})
                                    False
                                    [IVar (SrcSpanInfo {srcInfoSpan = SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 55, srcSpanStartColumn = 26, srcSpanEndLine = 55, srcSpanEndColumn = 44}, srcInfoPoints = []})
                                              (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 55, srcSpanStartColumn = 26, srcSpanEndLine = 55, srcSpanEndColumn = 44}, srcInfoPoints = []}) "unsafeInterleaveIO")])}
