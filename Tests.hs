import Language.Haskell.Imports.Fold as Fold (tests)
import Language.Haskell.Imports.SrcLoc as SrcLoc (tests)
import Test.HUnit (runTestTT, Test(TestList))

main =
    do counts <- runTestTT (TestList [Fold.tests, SrcLoc.tests])
       putStrLn (show counts)
