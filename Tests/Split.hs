module Tests.Split where

import Control.Monad as List (mapM_)
import Data.Set.Extra as Set (mapM_)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..), Name(Ident))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Internal (modifyParams, Params(testMode), runCleanT)
import Language.Haskell.Modules.ModuVerse (parseModule, putName)
import Language.Haskell.Modules.Params (modifyTestMode)
import Language.Haskell.Modules.SourceDirs (modulePathBase, SourceDirs(putDirs))
import Language.Haskell.Modules.Split (defaultSymbolToModule, splitModule, splitModuleDecls)
import Language.Haskell.Modules.Util.QIO (noisily)
import Language.Haskell.Modules.Util.Test (diff, repoModules)
import Prelude hiding (writeFile)
import System.Cmd (system)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Test.HUnit (assertEqual, Test(TestCase, TestList, TestLabel))

tests :: Test
tests = TestList [split2a, split2b, split4, split4b, split4c, split5]

slow :: Test
slow = TestList [split1]

split1 :: Test
split1 =
    TestCase $
      do _ <- system "rsync -aHxS --delete testdata/debian/ tmp"
         _ <- runCleanT $ noisily $ noisily $
           do putDirs ["tmp"]
              Set.mapM_ (\ name -> parseModule (modulePathBase "hs" name) >>= putName name) repoModules
              splitModuleDecls "Debian/Repo/Package.hs"
         (code, out, err) <- diff "testdata/split1-expected" "tmp"
         assertEqual "splitModule" (ExitSuccess, "", "") (code, out, err)

split2a :: Test
split2a =
    TestCase $
    do _ <- system "rsync -aHxS --delete testdata/split2/ tmp"
       _ <- runCleanT $ noisily $ noisily $
         do modifyParams (\ p -> p {testMode = True})
            putDirs ["tmp"]
            parseModule (modulePathBase "hs" (S.ModuleName "Split")) >>= putName (S.ModuleName "Split")
            splitModuleDecls "Split.hs"
       (code, out, err) <- diff "testdata/split2-expected" "tmp"
       assertEqual "split2" (ExitSuccess, "", "") (code, out, err)

split2b :: Test
split2b =
    TestCase $
    do _ <- system "rsync -aHxS --delete testdata/split2/ tmp"
       _ <- runCleanT $ noisily $ noisily $
         do putDirs ["tmp"]
            parseModule (modulePathBase "hs" (S.ModuleName "Split")) >>= putName (S.ModuleName "Split")
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
    do _ <- system "rsync -aHxs --delete testdata/split4/ tmp"
       _ <- withCurrentDirectory "tmp" $
         runCleanT $ noisily $ noisily $ modifyTestMode (const True) >> parseModule (modulePathBase "hs" (S.ModuleName "Split4")) >>= putName (S.ModuleName "Split4") >> splitModuleDecls "Split4.hs"
       result <- diff "testdata/split4-expected" "tmp"
       assertEqual "Split4" (ExitSuccess, "", "") result

split4b :: Test
split4b =
    TestLabel "Split4b" $ TestCase $
    do _ <- system "rsync -aHxs --delete testdata/split4/ tmp"
       _ <- withCurrentDirectory "tmp" $
         runCleanT $ noisily $ noisily $
           modifyTestMode (const True) >>
           parseModule (modulePathBase "hs" (S.ModuleName "Split4")) >>=
           putName (S.ModuleName "Split4") >>
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
    do _ <- system "rsync -aHxs --delete testdata/split4/ tmp"
       _ <- withCurrentDirectory "tmp" $
         runCleanT $ noisily $ noisily $
           modifyTestMode (const True) >>
           parseModule (modulePathBase "hs" (S.ModuleName "Split4")) >>=
           putName (S.ModuleName "Split4") >>
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
    do _ <- system "rsync -aHxs --delete testdata/split5/ tmp"
       _ <- withCurrentDirectory "tmp" $
         runCleanT $ noisily $ noisily $
           parseModule "B.hs" >>= \ b ->
           List.mapM_ (\ name -> parseModule (modulePathBase "hs" name) >>= putName name)
                      [S.ModuleName "A",
                       S.ModuleName "B",
                       S.ModuleName "C",
                       S.ModuleName "D",
                       S.ModuleName "E"] >>
           modifyTestMode (const True) >>
           splitModule (defaultSymbolToModule b) "B.hs"
       result <- diff "testdata/split5-expected" "tmp"
       assertEqual "Split5" (ExitSuccess, "", "") result
