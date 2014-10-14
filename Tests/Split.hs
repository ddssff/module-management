module Split where

import Control.Monad as List (mapM_)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..), Name(Ident))
import Language.Haskell.Modules (modifyTestMode, noisily, putDirs, putModule, runCleanT, splitModule, splitModuleDecls, withCurrentDirectory, findHsModules, extraImport)
import Language.Haskell.Modules.Util.Test (diff, repoModules, rsync)
import Prelude hiding (writeFile)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Test.HUnit (assertEqual, Test(TestCase, TestList, TestLabel))

tests :: Test
tests = TestList [split2a, split2b, split4, split4b, split4c, split5 {-, split6-}]

slow :: Test
slow = TestList [split1]

split1 :: Test
split1 =
    TestCase $
      do _ <- rsync "testdata/debian" "tmp"
         _ <- runCleanT $ noisily $ noisily $
           do putDirs ["tmp"]
              mapM_ putModule (map S.ModuleName repoModules)
              splitModuleDecls "tmp/Debian/Repo/Package.hs"
         (code, out, err) <- diff "testdata/split1-expected" "tmp"
         assertEqual "split1" (ExitSuccess, "", "") (code, out, err)

split2a :: Test
split2a =
    TestCase $
    do _ <- rsync "testdata/split2" "tmp"
       _ <- runCleanT $ noisily $ noisily $
         do modifyTestMode (const True)
            putDirs ["tmp"]
            putModule (S.ModuleName "Split")
            splitModuleDecls "tmp/Split.hs"
       (code, out, err) <- diff "testdata/split2-expected" "tmp"
       assertEqual "split2a" (ExitSuccess, "", "") (code, out, err)

split2b :: Test
split2b =
    TestCase $
    do _ <- rsync "testdata/split2" "tmp"
       _ <- runCleanT $ noisily $ noisily $
         do putDirs ["tmp"]
            putModule (S.ModuleName "Split")
            splitModuleDecls "tmp/Split.hs"
       (code, out, err) <- diff "testdata/split2-clean-expected" "tmp"
       -- The output of splitModule is "correct", but it will not be
       -- accepted by GHC until the fix for
       -- http://hackage.haskell.org/trac/ghc/ticket/8011 is
       -- available.
       assertEqual "split2b" (ExitFailure 1,"diff -ru '--exclude=*~' '--exclude=*.imports' testdata/split2-clean-expected/Split/Clean.hs tmp/Split/Clean.hs\n--- testdata/split2-clean-expected/Split/Clean.hs\n+++ tmp/Split/Clean.hs\n@@ -6,7 +6,7 @@\n     ) where\n \n import Data.Char (isAlphaNum)\n-import URL (ToURL(toURL), URLT)\n+import URL (ToURL(URLT, toURL))\n \n clean :: (ToURL url, Show (URLT url)) => url -> String\n clean = filter isAlphaNum . show . toURL\n", "") (code, out, err)

split4 :: Test
split4 =
    TestLabel "Split4" $ TestCase $
    do _ <- rsync "testdata/split4" "tmp"
       _ <- withCurrentDirectory "tmp" $
         runCleanT $ noisily $ noisily $
           modifyTestMode (const True) >>
           putModule (S.ModuleName "Split4") >>
           splitModuleDecls "Split4.hs"
       result <- diff "testdata/split4-expected" "tmp"
       assertEqual "split4" (ExitSuccess, "", "") result

split4b :: Test
split4b =
    TestLabel "Split4b" $ TestCase $
    do _ <- rsync "testdata/split4" "tmp"
       _ <- withCurrentDirectory "tmp" $
         runCleanT $ noisily $ noisily $
           modifyTestMode (const True) >>
           putModule (S.ModuleName "Split4") >>
           splitModule f "Split4.hs"
       result <- diff "testdata/split4b-expected" "tmp"
       assertEqual "split4b" (ExitSuccess, "", "") result
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
           putModule (S.ModuleName "Split4") >>
           splitModule f "Split4.hs"
       result <- diff "testdata/split4c-expected" "tmp"
       assertEqual "split4c" (ExitSuccess, "", "") result
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
           List.mapM_ (putModule . S.ModuleName) ["A", "B", "C", "D", "E"] >>
           modifyTestMode (const True) >>
           splitModuleDecls "B.hs"
       result <- diff "testdata/split5-expected" "tmp"
       assertEqual "split5" (ExitSuccess, "", "") result

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
       assertEqual "split6" (ExitSuccess, "", "") result
    where
      f (Just (S.Ident "countTasks")) = S.ModuleName "IO"
      f _ = S.ModuleName "Debian.Repo.Monads.Apt"

-- This code is, in some sense, inherently unsplittable.
split7 :: Test
split7 =
    TestLabel "split7" $ TestCase $
    do _ <- rsync "testdata/fold3b" "tmp"
       _ <- withCurrentDirectory "tmp" $ runCleanT $
            do putModule (S.ModuleName "Main")
               extraImport (S.ModuleName "Main.GetPasteById") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.GetRecentPastes") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.InitialCtrlVState") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.InsertPaste") (S.ModuleName "Main.Instances")
               splitModule f "Main.hs"
       result <- diff "testdata/split7-expected" "tmp"
       assertEqual "split7" (ExitSuccess, "", "") result
    where
      f (Just (S.Ident "appTemplate")) = S.ModuleName "Route"
      f (Just (S.Ident "Route")) = S.ModuleName "Route"
      f (Just (S.Ident "route")) = S.ModuleName "Route"
      f (Just (S.Ident "CtrlV")) = S.ModuleName "Route"
      f (Just (S.Ident "CtrlV'")) = S.ModuleName "Route"
      f (Just (S.Ident "CtrlVForm")) = S.ModuleName "Route"
      f (Just (S.Ident "CtrlVState")) = S.ModuleName "Route"
      f (Just (S.Ident "viewPastePage")) = S.ModuleName "Route"
      f (Just (S.Ident "viewRecentPage")) = S.ModuleName "Route"
      f (Just (S.Ident "newPastePage")) = S.ModuleName "Route"
      f (Just (S.Ident "main")) = S.ModuleName "Main"
      f (Just (S.Ident "PasteId")) = S.ModuleName "PasteId"
      f (Just (S.Ident "Format")) = S.ModuleName "Format"
      f (Just (S.Ident "PasteMeta")) = S.ModuleName "PasteMeta"
      f (Just (S.Ident "Paste")) = S.ModuleName "Paste"
      f (Just (S.Ident "initialCtrlVState,")) = S.ModuleName "InitialCtrlVState"
      f (Just (S.Ident "insertPaste")) = S.ModuleName "InsertPaste"
      f (Just (S.Ident "getPasteById")) = S.ModuleName "GetPasteById"
      f (Just (S.Ident "Limit")) = S.ModuleName "Limit"
      f (Just (S.Ident "Offset")) = S.ModuleName "Offset"
      f (Just (S.Ident "getRecentPastes")) = S.ModuleName "GetRecentPastes"
      f (Just (S.Ident "formatPaste")) = S.ModuleName "FormatPaste"
      f (Just (S.Ident "pasteForm")) = S.ModuleName "PasteForm"
      f Nothing = S.ModuleName "Route"
      f _ = S.ModuleName "Other"

split7b :: Test
split7b =
    TestLabel "split7b" $ TestCase $
    do _ <- rsync "testdata/fold3b" "tmp"
       _ <- withCurrentDirectory "tmp" $ runCleanT $
            do putModule (S.ModuleName "Main")
               extraImport (S.ModuleName "Main.GetPasteById") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.GetRecentPastes") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.InitialCtrlVState") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.InsertPaste") (S.ModuleName "Main.Instances")
               splitModuleDecls "Main.hs"
       result <- diff "testdata/split7-expected" "tmp"
       assertEqual "split7b" (ExitSuccess, "", "") result
