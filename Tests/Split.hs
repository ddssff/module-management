module Tests.Split where

import Control.Monad as List (mapM_)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..), Name(Ident))
import Language.Haskell.Modules (modifyTestMode, noisily, putDirs, putModule, runCleanT, splitModule, splitModuleDecls, withCurrentDirectory, findHsModules)
import Language.Haskell.Modules.Util.Test (diff, repoModules, rsync)
import Prelude hiding (writeFile)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Test.HUnit (assertEqual, Test(TestCase, TestList, TestLabel))

tests :: Test
tests = TestList [split2a, split2b, split4, split4b, split4c, split5, split6]

slow :: Test
slow = TestList [split1]

split1 :: Test
split1 =
    TestCase $
      do _ <- rsync "testdata/debian" "tmp"
         _ <- runCleanT $ noisily $ noisily $
           do putDirs ["tmp"]
              mapM_ putModule repoModules
              splitModuleDecls "Debian/Repo/Package.hs"
         (code, out, err) <- diff "testdata/split1-expected" "tmp"
         assertEqual "splitModule" (ExitSuccess, "", "") (code, out, err)

split2a :: Test
split2a =
    TestCase $
    do _ <- rsync "testdata/split2" "tmp"
       _ <- runCleanT $ noisily $ noisily $
         do modifyTestMode (const True)
            putDirs ["tmp"]
            putModule "Split"
            splitModuleDecls "Split.hs"
       (code, out, err) <- diff "testdata/split2-expected" "tmp"
       assertEqual "split2" (ExitSuccess, "", "") (code, out, err)

split2b :: Test
split2b =
    TestCase $
    do _ <- rsync "testdata/split2" "tmp"
       _ <- runCleanT $ noisily $ noisily $
         do putDirs ["tmp"]
            putModule "Split"
            splitModuleDecls "Split.hs"
       (code, out, err) <- diff "testdata/split2-clean-expected" "tmp"
       -- The output of splitModule is "correct", but it will not be
       -- accepted by GHC until the fix for
       -- http://hackage.haskell.org/trac/ghc/ticket/8011 is
       -- available.
       assertEqual "split2-clean" (ExitFailure 1,"diff -ru '--exclude=*~' '--exclude=*.imports' testdata/split2-clean-expected/Split/Clean.hs tmp/Split/Clean.hs\n--- testdata/split2-clean-expected/Split/Clean.hs\n+++ tmp/Split/Clean.hs\n@@ -6,7 +6,7 @@\n     ) where\n \n import Data.Char (isAlphaNum)\n-import URL (ToURL(toURL), URLT)\n+import URL (ToURL(URLT, toURL))\n \n clean :: (ToURL url, Show (URLT url)) => url -> String\n clean = filter isAlphaNum . show . toURL\n", "") (code, out, err)

split4 :: Test
split4 =
    TestLabel "Split4" $ TestCase $
    do _ <- rsync "testdata/split4" "tmp"
       _ <- withCurrentDirectory "tmp" $
         runCleanT $ noisily $ noisily $ modifyTestMode (const True) >> putModule "Split4" >> splitModuleDecls "Split4.hs"
       result <- diff "testdata/split4-expected" "tmp"
       assertEqual "Split4" (ExitSuccess, "", "") result

split4b :: Test
split4b =
    TestLabel "Split4b" $ TestCase $
    do _ <- rsync "testdata/split4" "tmp"
       _ <- withCurrentDirectory "tmp" $
         runCleanT $ noisily $ noisily $
           modifyTestMode (const True) >>
           putModule "Split4" >>
           splitModule f "Split4.hs"
       result <- diff "testdata/split4b-expected" "tmp"
       assertEqual "Split4" (ExitSuccess, "", "") result
    where
      f :: Maybe S.Name -> S.ModuleName
      f (Just (S.Ident "getPackages")) = S.ModuleName ("Split4.A")
      f _ = S.ModuleName ("Split4.B")


split4c :: Test
split4c =
    TestLabel "Split4b" $ TestCase $
    do _ <- rsync "testdata/split4" "tmp"
       _ <- withCurrentDirectory "tmp" $
         runCleanT $ noisily $ noisily $
           modifyTestMode (const True) >>
           putModule "Split4" >>
           splitModule f "Split4.hs"
       result <- diff "testdata/split4c-expected" "tmp"
       assertEqual "Split4" (ExitSuccess, "", "") result
    where
      f :: Maybe S.Name -> S.ModuleName
      f (Just (S.Ident "getPackages")) = S.ModuleName ("Split4.A")
      f _ = S.ModuleName "Split4"

-- Test what happens when a split module is re-exported
split5 :: Test
split5 =
    TestLabel "Split5" $ TestCase $
    do _ <- rsync "testdata/split5" "tmp"
       _ <- withCurrentDirectory "tmp" $
         runCleanT $ noisily $ noisily $
           List.mapM_ putModule ["A", "B", "C", "D", "E"] >>
           modifyTestMode (const True) >>
           splitModuleDecls "B.hs"
       result <- diff "testdata/split5-expected" "tmp"
       assertEqual "Split5" (ExitSuccess, "", "") result

split6 :: Test
split6 =
    TestLabel "Split6" $ TestCase $
    do _ <- rsync "testdata/debian" "tmp"
       _ <- withCurrentDirectory "tmp" $
         findHsModules ["Debian", "Text", "Tmp"] >>= \ modules ->
         runCleanT $ noisily $ noisily $
           mapM putModule modules >>
           splitModule f "Debian/Repo/Monads/Apt.hs"
       result <- diff "testdata/split6-expected" "tmp"
       assertEqual "Split6" (ExitSuccess, "", "") result
    where
      f (Just (S.Ident "countTasks")) = S.ModuleName "IO"
      f _ = S.ModuleName "Debian.Repo.Monads.Apt"
