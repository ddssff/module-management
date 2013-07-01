{-# LANGUAGE PackageImports #-}
module Tests.Imports where

import Control.Applicative ((<$>))
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (bracket, catch, throw)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Default (def, Default)
import Data.Function (on)
import Data.List (find, groupBy, intercalate, nub, nubBy, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (empty, member, Set, singleton, toList, union, unions)
import Language.Haskell.Exts.Annotated (ParseResult(..))
import Language.Haskell.Exts.Annotated.Simplify as S (sImportDecl, sImportSpec, sModuleName, sName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(DerivDecl), ImportDecl(..), ImportSpec(..), ImportSpecList(ImportSpecList), InstHead(..), Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName), QName(..), Type(..))
import Language.Haskell.Exts.Extension (Extension(PackageImports, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances))
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ImportDecl(importLoc, importModule, importSpecs), ModuleName(..), Name(..))
import Language.Haskell.Modules.Common (modulePathBase, withCurrentDirectory)
import Language.Haskell.Modules.Fold (ModuleInfo, foldDecls, foldExports, foldHeader, foldImports)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Internal (getParams, markForDelete, modifyParams, ModuleResult(..), MonadClean, Params(..), parseFile, parseFileWithComments, runMonadClean, scratchDir)
import Language.Haskell.Modules.Params (modifyTestMode)
import Language.Haskell.Modules.Util.DryIO (replaceFile, tildeBackup)
import Language.Haskell.Modules.Util.Symbols (symbols)
import Language.Haskell.Modules.Util.Test (diff, rsync)
import System.Cmd (system)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((<.>), (</>))
import System.Process (readProcessWithExitCode, showCommandForUser)
import Test.HUnit (assertEqual, Test(..))

tests :: Test
tests = TestLabel "Clean" (TestList [test1, test2, test3, test4, test5, test6])

test1 :: Test
test1 =
    TestLabel "imports1" $ TestCase
      (do rsync "testdata/debian" "tmp"
          let name = S.ModuleName "Debian.Repo.Types.PackageIndex"
          let base = modulePathBase name
          _ <- withCurrentDirectory "tmp" (runMonadClean (cleanImports base))
          (code, diff, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/debian" </> base, "tmp" </> base] ""
          assertEqual "cleanImports"
                         (ExitFailure 1,
                          ["@@ -22,13 +22,13 @@",
                           "     , prettyPkgVersion",
                           "     ) where",
                           " ",
                           "-import Data.Text (Text, map)",
                           "+import Data.Text (Text)",
                           " import Debian.Arch (Arch(..))",
                           " import qualified Debian.Control.Text as T (Paragraph)",
                           " import Debian.Relation (BinPkgName(..), SrcPkgName(..))",
                           " import qualified Debian.Relation as B (PkgName, Relations)",
                           " import Debian.Release (Section(..))",
                           "-import Debian.Repo.Orphans ({- instances -})",
                           "+import Debian.Repo.Orphans ()",
                           " import Debian.Version (DebianVersion, prettyDebianVersion)",
                           " import System.Posix.Types (FileOffset)",
                           " import Text.PrettyPrint.ANSI.Leijen ((<>), Doc, Pretty(pretty), text)"],
                          "")
                          (code, drop 2 (lines diff), err))

test2 :: Test
test2 =
    TestLabel "Imports.test2" $ TestCase
      (do rsync "testdata/debian" "tmp"
          let name = S.ModuleName "Debian.Repo.PackageIndex"
              base = modulePathBase name
          _ <- withCurrentDirectory "tmp" (runMonadClean (cleanImports base))
          (code, diff, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/debian" </> base, "tmp" </> base] ""
          assertEqual "cleanImports" (ExitSuccess, "", "") (code, diff, err))

-- | Can we handle a Main module in a file named something other than Main.hs?
test3 :: Test
test3 =
    TestLabel "imports3" $ TestCase
      (rsync "testdata/imports3" "tmp" >>
       runMonadClean (modifyParams (\ p -> p {sourceDirs = ["tmp"]}) >> cleanImports "tmp/NotMain.hs") >>
       assertEqual "module name" () ())

-- | Preserve imports with a "hiding" clause
test4 :: Test
test4 =
    TestLabel "imports4" $ TestCase
      (rsync "testdata/imports4" "tmp" >>
       runMonadClean (modifyParams (\ p -> p {sourceDirs = ["tmp"]}) >> cleanImports "tmp/Hiding.hs") >>
       -- Need to check the text of Hiding.hs, but at least this verifies that there was no crash
       assertEqual "module name" () ())

-- | Preserve imports used by a standalone deriving declaration
test5 :: Test
test5 =
    TestLabel "imports5" $ TestCase
      (do _ <- rsync "testdata/imports5" "tmp"
          _ <- runMonadClean (modifyParams (\ p -> p {extensions = extensions p ++ [StandaloneDeriving, TypeSynonymInstances, FlexibleInstances],
                                                  sourceDirs = ["tmp"]}) >>
                          cleanImports "tmp/Deriving.hs")
          (code, diff, err) <- diff "testdata/imports5" "tmp"
          assertEqual "standalone deriving"
                      (ExitFailure 1,
                       (unlines
                        ["@@ -1,7 +1,6 @@",
                         " module Deriving where",
                         " ",
                         "-import Data.Text (Text)",
                         "-import Debian.Control (Paragraph(..), Paragraph'(..), Field'(..))",
                         "+import Debian.Control (Field'(..), Paragraph(..))",
                         " ",
                         " deriving instance Show (Field' String)",
                         " deriving instance Show Paragraph"]),
                       "")
                      (code, unlines (drop 3 (lines diff)), err))

-- | Comment at EOF
test6 :: Test
test6 =
    TestLabel "imports6" $ TestCase
      (do _ <- rsync "testdata/imports6" "tmp"
          _ <- withCurrentDirectory "tmp" (runMonadClean (modifyTestMode (const True) >> cleanImports "EndComment.hs"))
          (code, diff, err) <- readProcessWithExitCode "diff" ["-ru", "imports6-expected", "tmp"] ""
          assertEqual "comment at end" "" diff)