module Merge where

import Control.Lens ((.=))
import Control.Monad as List (mapM_)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(ModuleName))
import Language.Haskell.Modules (mergeModules, noisily, putModule, runModuVerseT, withCurrentDirectory, findHsModules, sourceDirs)
import Language.Haskell.Modules.ModuVerse (CleanMode(DoClean))
import Language.Haskell.Modules.Util.Test (diff, repoModules, rsync)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Test.HUnit (assertEqual, Test(TestCase, TestList))

tests :: Test
tests = TestList [{-test1, test2, test3,-} test4, test5, test6]

test1 :: Test
test1 =
    TestCase $
      do _ <- rsync "tests/data/debian" "tmp"
         _result <- runModuVerseT $ noisily $ noisily $ noisily $
           do sourceDirs .= ["tmp"]
              mapM_ (putModule . S.ModuleName) repoModules
              mergeModules DoClean
                     [S.ModuleName "Debian.Repo.AptCache", S.ModuleName "Debian.Repo.AptImage"]
                     (S.ModuleName "Debian.Repo.Cache")
         (code, out, err) <- diff "tests/data/merge1-expected" "tmp"
         assertEqual "merge1" (ExitSuccess, "", "") (code, out, err)

test2 :: Test
test2 =
    TestCase $
      do _ <- rsync "tests/data/debian" "tmp"
         _result <- runModuVerseT $
           do sourceDirs .= ["tmp"]
              mapM_ (putModule . S.ModuleName) repoModules
              mergeModules DoClean
                     [S.ModuleName "Debian.Repo.Types.Slice", S.ModuleName "Debian.Repo.Types.Repo", S.ModuleName "Debian.Repo.Types.EnvPath"]
                     (S.ModuleName "Debian.Repo.Types.Common")
         (code, out, err) <- diff "tests/data/merge2-expected" "tmp"
         assertEqual "merge2" (ExitSuccess, "", "") (code, out, err)

test3 :: Test
test3 =
    TestCase $
      do _ <- rsync "tests/data/debian" "tmp"
         _result <- withCurrentDirectory "tmp" $
                   runModuVerseT $
           do mapM_ (putModule . S.ModuleName) repoModules
              mergeModules DoClean
                     [S.ModuleName "Debian.Repo.Types.Slice",
                      S.ModuleName "Debian.Repo.Types.Repo",
                      S.ModuleName "Debian.Repo.Types.EnvPath"]
                     (S.ModuleName "Debian.Repo.Types.Slice")
         (code, out, err) <- diff "tests/data/merge3-expected" "tmp"
         assertEqual "mergeModules3" (ExitSuccess, "", "") (code, out, err)

test4 :: Test
test4 =
    TestCase $
      do _ <- rsync "tests/data/merge4" "tmp"
         _ <- withCurrentDirectory "tmp" $ runModuVerseT $
              do mapM_ (putModule . S.ModuleName) ["In1", "In2", "M1"]
                 mergeModules DoClean [S.ModuleName "In1", S.ModuleName "In2"] (S.ModuleName "Out")
         (code, out, err) <- diff "tests/data/merge4-expected" "tmp"
         assertEqual "mergeModules4" (ExitSuccess, "", "") (code, out, err)

test5 :: Test
test5 =
    TestCase $
      do _ <- rsync "tests/data/merge5" "tmp"
         _ <- withCurrentDirectory "tmp" $ runModuVerseT $ noisily $ noisily $ noisily $
              do List.mapM_ (putModule . S.ModuleName)
                            ["Apt.AptIO", "Apt.AptIOT", "Apt.AptState",
                             "Apt.InitState", "Apt.Instances", "Apt.MonadApt"]
                 mergeModules DoClean
                              [S.ModuleName "Apt.AptIO", S.ModuleName "Apt.AptIOT", S.ModuleName "Apt.AptState",
                               S.ModuleName "Apt.InitState", S.ModuleName "Apt.Instances", S.ModuleName "Apt.MonadApt"]
                              (S.ModuleName "Apt")
         (code, out, err) <- diff "tests/data/merge5-expected" "tmp"
         assertEqual "mergeModules5" (ExitFailure 1,"Only in tmp: Apt\n","") (code, out, err)

test6 :: Test
test6 =
    TestCase $
      do _ <- rsync "tests/data/merge6" "tmp"
         _ <- withCurrentDirectory "tmp" $
              findHsModules ["Test.hs", "A.hs", "B/C.hs", "B/D.hs"] >>= \ modules ->
              runModuVerseT $ noisily $ noisily $ noisily $
              do mapM_ putModule modules
                 mergeModules DoClean [S.ModuleName "B.C", S.ModuleName "A"] (S.ModuleName "A")
         (code, out, err) <- diff "tests/data/merge6-expected" "tmp"
         assertEqual "merge6" (ExitSuccess, "", "") (code, out, err)
