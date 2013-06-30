module Tests.Merge where

import Control.Monad as List (mapM)
import Control.Monad.Trans (liftIO)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.List as List (intercalate, map)
import Data.Map as Map (fromList, insert, lookup, Map, member, toAscList)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (difference, fromList, insert, Set, union)
import Data.Set.Extra as Set (mapM)
import Language.Haskell.Exts.Annotated.Simplify (sDecl, sExportSpec, sImportDecl, sModuleName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ExportSpecList(ExportSpecList), ImportDecl(..), Module(Module), ModuleHead(ModuleHead))
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Parser (fromParseResult)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(EModuleContents), ImportDecl(..), ModuleName(..))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Fold (ModuleInfo, echo, echo2, foldDecls, foldExports, foldHeader, foldImports, ignore, ignore2)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Internal (doResult, getParams, modifyParams, modulePath, ModuleResult(..), MonadClean, Params(sourceDirs, moduVerse, testMode), parseFileWithComments, runMonadClean)
import Language.Haskell.Modules.Merge (mergeModules)
import Language.Haskell.Modules.Util.Test (diff, rsync, repoModules)
import System.Cmd (system)
import System.Exit (ExitCode(ExitSuccess))
import Test.HUnit (assertEqual, Test(TestCase, TestList))

tests :: Test
tests = TestList [test1, test2, test3]

test1 :: Test
test1 =
    TestCase $
      do _ <- rsync "testdata/debian" "tmp"
         _result <- runMonadClean $
           do modifyParams (\ p -> p {sourceDirs = ["tmp"], moduVerse = Just repoModules})
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
           do modifyParams (\ p -> p {sourceDirs = ["tmp"], moduVerse = Just repoModules})
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
           do modifyParams (\ p -> p {moduVerse = Just repoModules})
              mergeModules
                     [S.ModuleName "Debian.Repo.Types.Slice",
                      S.ModuleName "Debian.Repo.Types.Repo",
                      S.ModuleName "Debian.Repo.Types.EnvPath"]
                     (S.ModuleName "Debian.Repo.Types.Slice")
         (code, out, err) <- diff "testdata/merge3-expected" "tmp"
         assertEqual "mergeModules3" (ExitSuccess, "", "") (code, out, err)
