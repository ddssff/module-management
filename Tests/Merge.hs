module Tests.Merge where

import Control.Monad as List (mapM_)
import Data.Set.Extra as Set (mapM_)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(ModuleName))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Internal (runCleanT)
import Language.Haskell.Modules.Merge (mergeModules)
import Language.Haskell.Modules.ModuVerse (parseModule, putName)
import Language.Haskell.Modules.Params (modifyTestMode)
import Language.Haskell.Modules.SourceDirs (modulePathBase, SourceDirs(putDirs))
import Language.Haskell.Modules.Util.QIO (noisily)
import Language.Haskell.Modules.Util.Test (diff, repoModules, rsync)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Test.HUnit (assertEqual, Test(TestCase, TestList))

tests :: Test
tests = TestList [test1, test2, test3, test4, test5]

test1 :: Test
test1 =
    TestCase $
      do _ <- rsync "testdata/debian" "tmp"
         _result <- runCleanT $
           do putDirs ["tmp"]
              modifyTestMode (const True)
              Set.mapM_ (\ name -> parseModule (modulePathBase "hs" name) >>= putName name) repoModules
              mergeModules
                     [S.ModuleName "Debian.Repo.AptCache", S.ModuleName "Debian.Repo.AptImage"]
                     (S.ModuleName "Debian.Repo.Cache")
         (code, out, err) <- diff "testdata/merge1-expected" "tmp"
         assertEqual "merge1" (ExitSuccess, "", "") (code, out, err)

test2 :: Test
test2 =
    TestCase $
      do _ <- rsync "testdata/debian" "tmp"
         _result <- runCleanT $
           do putDirs ["tmp"]
              modifyTestMode (const True)
              Set.mapM_ (\ name -> parseModule (modulePathBase "hs" name) >>= putName name) repoModules
              mergeModules
                     [S.ModuleName "Debian.Repo.Types.Slice", S.ModuleName "Debian.Repo.Types.Repo", S.ModuleName "Debian.Repo.Types.EnvPath"]
                     (S.ModuleName "Debian.Repo.Types.Common")
         (code, out, err) <- diff "testdata/merge2-expected" "tmp"
         assertEqual "merge2" (ExitSuccess, "", "") (code, out, err)

test3 :: Test
test3 =
    TestCase $
      do _ <- rsync "testdata/debian" "tmp"
         _result <- withCurrentDirectory "tmp" $
                   runCleanT $
           do modifyTestMode (const True)
              Set.mapM_ (\ name -> parseModule (modulePathBase "hs" name) >>= putName name) repoModules
              mergeModules
                     [S.ModuleName "Debian.Repo.Types.Slice",
                      S.ModuleName "Debian.Repo.Types.Repo",
                      S.ModuleName "Debian.Repo.Types.EnvPath"]
                     (S.ModuleName "Debian.Repo.Types.Slice")
         (code, out, err) <- diff "testdata/merge3-expected" "tmp"
         assertEqual "mergeModules3" (ExitSuccess, "", "") (code, out, err)

test4 :: Test
test4 =
    TestCase $
      do _ <- rsync "testdata/merge4" "tmp"
         _ <- withCurrentDirectory "tmp" $ runCleanT $
              do List.mapM_ (\ name -> parseModule (modulePathBase "hs" name) >>= putName name) [S.ModuleName "In1", S.ModuleName "In2", S.ModuleName "M1"]
                 mergeModules [S.ModuleName "In1", S.ModuleName "In2"] (S.ModuleName "Out")
         (code, out, err) <- diff "testdata/merge4-expected" "tmp"
         assertEqual "mergeModules4" (ExitSuccess, "", "") (code, out, err)

test5 :: Test
test5 =
    TestCase $
      do _ <- rsync "testdata/merge5" "tmp"
         _ <- withCurrentDirectory "tmp" $ runCleanT $ noisily $ noisily $ noisily $
              do modifyTestMode (const True)
                 List.mapM_ (\ name -> parseModule (modulePathBase "hs" name) >>= putName name)
                            [S.ModuleName "Apt.AptIO", S.ModuleName "Apt.AptIOT", S.ModuleName "Apt.AptState",
                             S.ModuleName "Apt.InitState", S.ModuleName "Apt.Instances", S.ModuleName "Apt.MonadApt"]
                 mergeModules [S.ModuleName "Apt.AptIO", S.ModuleName "Apt.AptIOT", S.ModuleName "Apt.AptState",
                               S.ModuleName "Apt.InitState", S.ModuleName "Apt.Instances", S.ModuleName "Apt.MonadApt"]
                              (S.ModuleName "Apt")
         (code, out, err) <- diff "testdata/merge5-expected" "tmp"
         assertEqual "mergeModules5" (ExitFailure 1,"Only in tmp: Apt\n","") (code, out, err)
