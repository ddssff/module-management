import Language.Haskell.Imports.Common as Common (tests)
import Language.Haskell.Imports.SrcLoc as SrcLoc (tests)
import Test.HUnit (runTestTT, Test(TestList))

main =
    do counts <- runTestTT (TestList [Common.tests, SrcLoc.tests])
       putStrLn (show counts)
