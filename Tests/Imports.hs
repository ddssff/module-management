{-# LANGUAGE CPP, PackageImports #-}
module Imports where


import qualified Language.Haskell.Exts.Syntax as S (ModuleName(ModuleName))
import Language.Haskell.Modules (cleanImports, modifyExtensions, modifyTestMode, modulePathBase, putDirs, runImportsT, withCurrentDirectory)
import Language.Haskell.Modules.Params (modifyParams, Params(hsFlags))
import Language.Haskell.Modules.SourceDirs (RelPath(unRelPath))
import Language.Haskell.Modules.Util.Test (diff, rsync)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual, Test(..))

#if MIN_VERSION_haskell_src_exts(1,14,0)
import Language.Haskell.Exts.Extension (KnownExtension(FlexibleInstances, StandaloneDeriving, TypeSynonymInstances),Extension(EnableExtension))
nameToExtension x = EnableExtension x

#else
import Language.Haskell.Exts.Extension (Extension(FlexibleInstances, StandaloneDeriving, TypeSynonymInstances))
nameToExtension x = x

#endif

tests :: Test
tests = TestLabel "Clean" (TestList [test1, {-test2,-} test3, test4, test5, test6 {-, test7-}])

test1 :: Test
test1 =
    TestLabel "imports1" $ TestCase
      (do rsync "testdata/debian" "tmp"
          let name = S.ModuleName "Debian.Repo.PackageIndex"
          let base = modulePathBase "hs" name
          _ <- withCurrentDirectory "tmp" $ (runImportsT (cleanImports [unRelPath base]))
          (code, out, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/debian" </> unRelPath base, "tmp" </> unRelPath base] ""
          assertEqual "cleanImports"
                         (ExitFailure 1,
                          ["@@ -28,15 +28,15 @@",
                           "     , sortBinaryPackages",
                           "     ) where",
                           " ",
                           "-import Data.List as List (sortBy, map, filter)",
                           "+import Data.List as List (filter, map, sortBy)",
                           " import Data.Monoid ((<>))",
                           "-import Data.Set as Set (map, filter, toList, unions)",
                           "+import Data.Set as Set (filter, map, toList, unions)",
                           " import Data.Text (Text)",
                           "-import Debian.Arch (Arch(..), ArchOS(..), ArchCPU(..), prettyArch)",
                           "-import qualified Debian.Control.Text as T",
                           "+import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..), prettyArch)",
                           "+import qualified Debian.Control.Text as T (Paragraph)",
                           "+import Debian.Pretty (PP(..), ppPrint)",
                           " import Debian.Relation (BinPkgName(..), SrcPkgName(..))",
                           " import qualified Debian.Relation as B (Relations)",
                           "-import Debian.Pretty (PP(..), ppPrint)",
                           " import Debian.Release (releaseName', Section(..), sectionName')",
                           " import Debian.Repo.PackageID (PackageID(packageName, packageVersion), prettyPackageID)",
                           " import Debian.Repo.Release (Release(..))"],
                          "")
                          (code, drop 2 (lines out), err))

test2 :: Test
test2 =
    TestLabel "Imports.test2" $ TestCase
      (do rsync "testdata/debian" "tmp"
          let name = S.ModuleName "Debian.Repo.PackageIndex"
              base = modulePathBase "hs" name
          _ <- withCurrentDirectory "tmp" (runImportsT (cleanImports [unRelPath base]))
          (code, out, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/debian" </> unRelPath base, "tmp" </> unRelPath base] ""
          assertEqual "cleanImports" (ExitSuccess, "", "") (code, out, err))

-- | Can we handle a Main module in a file named something other than Main.hs?
test3 :: Test
test3 =
    TestLabel "imports3" $ TestCase
      (rsync "testdata/imports3" "tmp" >>
       runImportsT (putDirs ["tmp"] >> cleanImports ["tmp/NotMain.hs"]) >>
       assertEqual "module name" () ())

-- | Preserve imports with a "hiding" clause
test4 :: Test
test4 =
    TestLabel "imports4" $ TestCase
      (rsync "testdata/imports4" "tmp" >>
       runImportsT (putDirs ["tmp"] >> cleanImports ["tmp/Hiding.hs"]) >>
       -- Need to check the text of Hiding.hs, but at least this verifies that there was no crash
       assertEqual "module name" () ())

-- | Preserve imports used by a standalone deriving declaration
test5 :: Test
test5 =
    TestLabel "imports5" $ TestCase
      (do _ <- rsync "testdata/imports5" "tmp"
          _ <- runImportsT
                 (putDirs ["tmp"] >>
                  modifyExtensions (++ map nameToExtension [StandaloneDeriving, TypeSynonymInstances, FlexibleInstances]) >>
                  cleanImports ["tmp/Deriving.hs"])
          (code, out, err) <- diff "testdata/imports5" "tmp"
          assertEqual "standalone deriving"
                      (ExitFailure 1,
                       (unlines
                        ["@@ -1,7 +1,6 @@",
                         " module Deriving where",
                         " ",
                         "-import Data.Text (Text)",
                         "-import Debian.Control (Paragraph(..), Paragraph'(..), Field'(..))",
                         "+",
                         " ",
                         " {-",
                         " deriving instance Show (Field' String)"]),
                       "")
                      (code, unlines (drop 3 (lines out)), err))

-- | Comment at EOF
test6 :: Test
test6 =
    TestLabel "imports6" $ TestCase
      (do _ <- rsync "testdata/imports6" "tmp"
          _ <- withCurrentDirectory "tmp" (runImportsT (modifyTestMode (const True) >> cleanImports ["EndComment.hs"]))
          (_, out, _) <- readProcessWithExitCode "diff" ["-ru", "imports6-expected", "tmp"] ""
          assertEqual "comment at end" "" out)

-- Clean a file with tabs
test7 :: Test
test7 =
    TestLabel "imports7" $ TestCase
      (do _ <- rsync "testdata/imports7" "tmp"
          _ <- withCurrentDirectory "tmp" (runImportsT (putDirs [".", ".."] >>
                                                      modifyParams (\ m -> m {hsFlags = "-DMIN_VERSION_haskell_src_exts(a,b,c)=1" : hsFlags m}) >>
                                                      modifyTestMode (const True) >>
                                                      cleanImports ["CLI.hs"]))
          out <- diff "testdata/imports7-expected" "tmp"
          assertEqual "CLI" (ExitSuccess, "", "") out)
