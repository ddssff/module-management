{-# LANGUAGE CPP, PackageImports #-}
module Imports where

import Control.Lens ((%=))
import Language.Haskell.Exts.Extension (KnownExtension(FlexibleInstances, StandaloneDeriving, TypeSynonymInstances),Extension(EnableExtension))
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(ModuleName))
import Language.Haskell.Modules (cleanImports, modifyExtensions, modulePathBase, putHsSourceDirs, runImportsT, withCurrentDirectory)
import Language.Haskell.Modules.Params (modifyParams, Params, hsFlags, CleanMode(DoClean))
import Language.Haskell.Modules.SourceDirs (RelPath(unRelPath))
import Language.Haskell.Modules.Util.Test (diff, rsync)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual, Test(..))

nameToExtension x = EnableExtension x

tests :: Test
tests = TestLabel "Clean" (TestList [test1, test3, test4, test5, test6, test7])

tmp :: FilePath
tmp = "tests/tmp"

test1 :: Test
test1 =
    TestLabel "imports1" $ TestCase
      (do let expected = "tests/data"
          createDirectoryIfMissing True tmp
          rsync "tests/data" tmp
          let base = modulePathBase "hs" (S.ModuleName "Test1")
          _ <- withCurrentDirectory tmp $ (runImportsT (cleanImports [unRelPath base]))
          (code, out, err) <- readProcessWithExitCode "diff" ["-ru", expected </> unRelPath base, tmp </> unRelPath base] ""
          assertEqual "cleanImports"
                         (ExitFailure 1,
                          [ "@@ -21,17 +21,18 @@",
                            "     , friendlyNames",
                            "     ) where",
                            " ",
                            "+import Control.Lens (Field2(_2), Field3(_3), view)",
                            " import Control.Lens hiding (cons)",
                            " import Control.Monad (foldM)",
                            " import Data.Generics (Data, everywhere, mkT)",
                            "-import Data.Graph as Graph",
                            "-import Data.Map as Map (Map, fromList, toList)",
                            "+import Data.Graph as Graph (Graph, reachable, Vertex)",
                            "+import Data.Map as Map (fromList, Map, toList)",
                            " import Data.Maybe (fromJust, fromMaybe)",
                            " import Data.Set as Set (fromList, Set, toList)",
                            "-import Language.Haskell.TH",
                            "-import Language.Haskell.TH.PprLib",
                            "+import Language.Haskell.TH (Con(..), Dec(..), Info(PrimTyConI), mkName, Name, nameBase, Ppr(ppr), Type(AppT, ConT, ForallT))",
                            "+import Language.Haskell.TH.PprLib (ptext, to_HPJ_Doc)",
                            " import Language.Haskell.TH.Syntax (Lift(lift), Name(Name), NameFlavour(NameS), Quasi(qReify), StrictType, VarStrictType)",
                            "-import qualified Text.PrettyPrint as HPJ",
                            "+import qualified Text.PrettyPrint as HPJ (Mode(LeftMode, OneLineMode), renderStyle, style, Style(lineLength, mode))",
                            " ",
                            " instance Ppr () where",
                            "     ppr () = ptext \"()\""],
                          "")
                          (code, drop 2 (lines out), err))

-- | Can we handle a Main module in a file named something other than Main.hs?
test3 :: Test
test3 =
    TestLabel "imports3" $ TestCase
      (rsync "tests/data/imports3" tmp >>
       runImportsT (putHsSourceDirs [tmp] >> cleanImports [tmp </> "NotMain.hs"]) >>
       assertEqual "module name" () ())

-- | Preserve imports with a "hiding" clause
test4 :: Test
test4 =
    TestLabel "imports4" $ TestCase
      (rsync "tests/data/imports4" tmp >>
       runImportsT (putHsSourceDirs [tmp] >> cleanImports [tmp </> "Hiding.hs"]) >>
       -- Need to check the text of Hiding.hs, but at least this verifies that there was no crash
       assertEqual "module name" () ())

-- | Preserve imports used by a standalone deriving declaration
test5 :: Test
test5 =
    TestLabel "imports5" $ TestCase
      (do _ <- rsync "tests/data/imports5" tmp
          _ <- runImportsT
                 (putHsSourceDirs [tmp] >>
                  modifyExtensions (++ map nameToExtension [StandaloneDeriving, TypeSynonymInstances, FlexibleInstances]) >>
                  cleanImports [tmp </> "Deriving.hs"])
          (code, out, err) <- diff "tests/data/imports5" tmp
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
      (do _ <- rsync "tests/data/imports6" tmp
          _ <- withCurrentDirectory tmp (runImportsT (cleanImports ["EndComment.hs"]))
          (_, out, _) <- readProcessWithExitCode "diff" ["-ru", "tests/data/imports6-expected", tmp] ""
          assertEqual "comment at end" "Only in tests/tmp: EndComment.hs~\n" out)

-- Clean a file with tabs
test7 :: Test
test7 =
    TestLabel "imports7" $ TestCase
      (do _ <- rsync "tests/data/imports7" tmp
          _ <- withCurrentDirectory tmp (runImportsT (do putHsSourceDirs [".", "../.."]
                                                         hsFlags %= (\x -> "-DMIN_VERSION_haskell_src_exts(a,b,c)=1" : x)
                                                         cleanImports ["CLI.hs"]))
          out <- diff "tests/data/imports7-expected" tmp
          assertEqual "CLI" (ExitSuccess, "", "") out)
