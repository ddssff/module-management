module Tests.Split where

import Data.Set as Set (empty, singleton)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..), Name(Ident))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Internal (modifyParams, Params(moduVerse, sourceDirs, testMode), runMonadClean)
import Language.Haskell.Modules.Params (modifyModuVerse, modifyTestMode)
import Language.Haskell.Modules.Split (DeclName(..), splitModule, splitModuleDecls)
import Language.Haskell.Modules.Util.QIO (noisily)
import Language.Haskell.Modules.Util.Test (diff, repoModules)
import Prelude hiding (writeFile)
import System.Cmd (system)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Test.HUnit (assertEqual, Test(TestCase, TestList, TestLabel))

tests :: Test
tests = TestList [split1, split2a, split2b, split4, split4b]

split1 :: Test
split1 =
    TestCase $
      do _ <- system "rsync -aHxS --delete testdata/debian/ tmp"
         runMonadClean $ noisily $ noisily $
           do modifyParams (\ p -> p {sourceDirs = ["tmp"], moduVerse = Just repoModules})
              splitModuleDecls (S.ModuleName "Debian.Repo.Package")
         (code, out, err) <- diff "testdata/split1-expected" "tmp"
         assertEqual "splitModule" (ExitSuccess, "", "") {- (ExitFailure 1, "diff -ru '--exclude=*~' '--exclude=*.imports' testdata/split1-expected/Debian/Repo/Package/BinaryPackagesOfIndex.hs tmp/Debian/Repo/Package/BinaryPackagesOfIndex.hs\n--- testdata/split1-expected/Debian/Repo/Package/BinaryPackagesOfIndex.hs\n+++ tmp/Debian/Repo/Package/BinaryPackagesOfIndex.hs\n@@ -22,4 +22,4 @@\n binaryPackagesOfIndex repo release index =\n     case packageIndexArch index of\n       Source -> return (Right [])\n-      _ -> liftIO $ getPackages repo release index -- >>= return . either Left (Right . List.map (toBinaryPackage index . packageInfo))\n\\ No newline at end of file\n+      _ -> liftIO $ getPackages repo release index \n\\ No newline at end of file\n", "") -} (code, out, err)

split2a :: Test
split2a =
    TestCase $
    do _ <- system "rsync -aHxS --delete testdata/split2/ tmp"
       runMonadClean $
         do modifyParams (\ p -> p {testMode = True,
                                    sourceDirs = ["tmp"],
                                    -- extensions = NoImplicitPrelude : extensions p,
                                    moduVerse = Just (singleton (S.ModuleName "Split"))})
            splitModuleDecls (S.ModuleName "Split")
       (code, out, err) <- diff "testdata/split2-expected" "tmp"
       assertEqual "split2" (ExitSuccess, "", "") (code, out, err)

split2b :: Test
split2b =
    TestCase $
    do _ <- system "rsync -aHxS --delete testdata/split2/ tmp"
       runMonadClean $
         do modifyParams (\ p -> p {testMode = False,
                                    sourceDirs = ["tmp"],
                                    -- extensions = NoImplicitPrelude : extensions p,
                                    moduVerse = Just (singleton (S.ModuleName "Split"))})
            splitModuleDecls (S.ModuleName "Split")
       (code, out, err) <- diff "testdata/split2-clean-expected" "tmp"
       -- The output of splitModule is "correct", but it will not be
       -- accepted by GHC until the fix for
       -- http://hackage.haskell.org/trac/ghc/ticket/8011 is
       -- available.
       assertEqual "split2-clean" (ExitFailure 1,"diff -ru '--exclude=*~' '--exclude=*.imports' testdata/split2-clean-expected/Split/Clean.hs tmp/Split/Clean.hs\n--- testdata/split2-clean-expected/Split/Clean.hs\n+++ tmp/Split/Clean.hs\n@@ -7,7 +7,7 @@\n \n \n import Data.Char (isAlphaNum)\n-import URL (ToURL(toURL), URLT)\n+import URL (ToURL(URLT, toURL))\n \n clean :: (ToURL url, Show (URLT url)) => url -> String\n clean = filter isAlphaNum . show . toURL\n", "") (code, out, err)

split4 :: Test
split4 =
    TestLabel "Split4" $ TestCase $
    do system "rsync -aHxs --delete testdata/split4/ tmp"
       withCurrentDirectory "tmp" $
         runMonadClean $ modifyTestMode (const True) >> modifyModuVerse (const Set.empty) >> splitModuleDecls (S.ModuleName "Split4")
       result <- diff "testdata/split4-expected" "tmp"
       assertEqual "Split4" (ExitSuccess, "", "") result

split4b :: Test
split4b =
    TestLabel "Split4b" $ TestCase $
    do system "rsync -aHxs --delete testdata/split4/ tmp"
       withCurrentDirectory "tmp" $
         runMonadClean $ modifyTestMode (const True) >> modifyModuVerse (const Set.empty) >> splitModule f (S.ModuleName "Split4")
       result <- diff "testdata/split4b-expected" "tmp"
       assertEqual "Split4" (ExitSuccess, "", "") result
    where
      f :: DeclName -> S.ModuleName
      f x@(Exported (S.Ident "getPackages")) = S.ModuleName "Split4.A"
      f x = S.ModuleName "Split4.B"

