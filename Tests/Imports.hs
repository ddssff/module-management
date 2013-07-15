{-# LANGUAGE PackageImports #-}
module Tests.Imports where

import Language.Haskell.Exts.Extension (Extension(FlexibleInstances, StandaloneDeriving, TypeSynonymInstances))
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(ModuleName))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Internal (runCleanT)
import Language.Haskell.Modules.ModuVerse (modifyExtensions)
import Language.Haskell.Modules.Params (modifyTestMode)
import Language.Haskell.Modules.SourceDirs (modulePathBase, putDirs, RelPath(unRelPath))
import Language.Haskell.Modules.Util.Test (diff, rsync)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual, Test(..))

tests :: Test
tests = TestLabel "Clean" (TestList [test1, test2, test3, test4, test5, test6, test7])

test1 :: Test
test1 =
    TestLabel "imports1" $ TestCase
      (do rsync "testdata/debian" "tmp"
          let name = S.ModuleName "Debian.Repo.Types.PackageIndex"
          let base = modulePathBase "hs" name
          _ <- withCurrentDirectory "tmp" (runCleanT (cleanImports [unRelPath base]))
          (code, out, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/debian" </> unRelPath base, "tmp" </> unRelPath base] ""
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
                          (code, drop 2 (lines out), err))

test2 :: Test
test2 =
    TestLabel "Imports.test2" $ TestCase
      (do rsync "testdata/debian" "tmp"
          let name = S.ModuleName "Debian.Repo.PackageIndex"
              base = modulePathBase "hs" name
          _ <- withCurrentDirectory "tmp" (runCleanT (cleanImports [unRelPath base]))
          (code, out, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/debian" </> unRelPath base, "tmp" </> unRelPath base] ""
          assertEqual "cleanImports" (ExitSuccess, "", "") (code, out, err))

-- | Can we handle a Main module in a file named something other than Main.hs?
test3 :: Test
test3 =
    TestLabel "imports3" $ TestCase
      (rsync "testdata/imports3" "tmp" >>
       runCleanT (putDirs ["tmp"] >> cleanImports ["NotMain.hs"]) >>
       assertEqual "module name" () ())

-- | Preserve imports with a "hiding" clause
test4 :: Test
test4 =
    TestLabel "imports4" $ TestCase
      (rsync "testdata/imports4" "tmp" >>
       runCleanT (putDirs ["tmp"] >> cleanImports ["Hiding.hs"]) >>
       -- Need to check the text of Hiding.hs, but at least this verifies that there was no crash
       assertEqual "module name" () ())

-- | Preserve imports used by a standalone deriving declaration
test5 :: Test
test5 =
    TestLabel "imports5" $ TestCase
      (do _ <- rsync "testdata/imports5" "tmp"
          _ <- runCleanT
                 (putDirs ["tmp"] >>
                  modifyExtensions (++ [StandaloneDeriving, TypeSynonymInstances, FlexibleInstances]) >>
                  cleanImports ["Deriving.hs"])
          (code, out, err) <- diff "testdata/imports5" "tmp"
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
                      (code, unlines (drop 3 (lines out)), err))

-- | Comment at EOF
test6 :: Test
test6 =
    TestLabel "imports6" $ TestCase
      (do _ <- rsync "testdata/imports6" "tmp"
          _ <- withCurrentDirectory "tmp" (runCleanT (modifyTestMode (const True) >> cleanImports ["EndComment.hs"]))
          (_, out, _) <- readProcessWithExitCode "diff" ["-ru", "imports6-expected", "tmp"] ""
          assertEqual "comment at end" "" out)

-- Clean a file with tabs
test7 :: Test
test7 =
    TestLabel "imports7" $ TestCase
      (do _ <- rsync "testdata/imports7" "tmp"
          _ <- withCurrentDirectory "tmp" (runCleanT (putDirs [".", ".."] >> modifyTestMode (const True) >> cleanImports ["CLI.hs"]))
          out <- diff "testdata/imports7-expected" "tmp"
          assertEqual "CLI" (ExitSuccess, "", "") out)
