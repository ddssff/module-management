module Tests.Merge where

import Control.Monad as List (mapM_)
import Control.Monad.Trans (liftIO)
import Data.Set.Extra as Set
import Language.Haskell.Exts.Extension (Extension(..))
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(ModuleName))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Internal (runMonadClean)
import Language.Haskell.Modules.Merge (mergeModules)
import Language.Haskell.Modules.ModuVerse (putName, modifySourceDirs, parseModule, modulePath, modifyExtensions)
import Language.Haskell.Modules.Params (modifyTestMode)
import Language.Haskell.Modules.Util.Test (diff, repoModules, rsync, findModules)
import System.Exit (ExitCode(ExitSuccess))
import Test.HUnit (assertEqual, Test(TestCase, TestList))

tests :: Test
tests = TestList [test1, test2, test3, test4]

test1 :: Test
test1 =
    TestCase $
      do _ <- rsync "testdata/debian" "tmp"
         _result <- runMonadClean $
           do modifySourceDirs (const ["tmp"])
              modifyTestMode (const True)
              Set.mapM_ (\ name -> modulePath name >>= parseModule >>= putName name) repoModules
              mergeModules
                     [S.ModuleName "Debian.Repo.AptCache", S.ModuleName "Debian.Repo.AptImage"]
                     (S.ModuleName "Debian.Repo.Cache")
         (code, out, err) <- diff "testdata/merge1-expected" "tmp"
         assertEqual "merge1" (ExitSuccess, "", "") (code, out, err)

test2 :: Test
test2 =
    TestCase $
      do _ <- rsync "testdata/debian" "tmp"
         _result <- runMonadClean $
           do modifySourceDirs (const ["tmp"])
              modifyTestMode (const True)
              Set.mapM_ (\ name -> modulePath name >>= parseModule >>= putName name) repoModules
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
                   runMonadClean $
           do modifyTestMode (const True)
              Set.mapM_ (\ name -> modulePath name >>= parseModule >>= putName name) repoModules
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
         _ <- withCurrentDirectory "tmp" $ runMonadClean $
              do List.mapM_ (\ name -> modulePath name >>= parseModule >>= putName name) [S.ModuleName "In1", S.ModuleName "In2", S.ModuleName "M1"]
                 mergeModules [S.ModuleName "In1", S.ModuleName "In2"] (S.ModuleName "Out")
         (code, out, err) <- diff "testdata/merge4-expected" "tmp"
         assertEqual "mergeModules4" (ExitSuccess, "", "") (code, out, err)
