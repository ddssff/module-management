module Tests.Split where

import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Default (Default(def))
import Data.List as List (filter, intercalate, map, nub)
import Data.Map as Map (delete, elems, empty, filter, insertWith, lookup, Map, mapWithKey)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (delete, difference, empty, filter, fold, insert, intersection, map, member, null, Set, singleton, toList, union, unions)
import Data.Set.Extra as Set (gFind, mapM)
import Language.Haskell.Exts (fromParseResult, ParseResult(ParseOk, ParseFailed))
import qualified Language.Haskell.Exts.Annotated as A (Decl, ImportDecl(..), ImportSpecList(..), Module(Module), ModuleHead(ModuleHead), Name)
import Language.Haskell.Exts.Annotated.Simplify (sImportDecl, sImportSpec, sModuleName, sName)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrint, prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ImportDecl(..), ModuleName(..), Name(..))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Fold (ModuleInfo, echo, echo2, foldDecls, foldExports, foldHeader, foldImports, foldModule, ignore, ignore2)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Internal (doResult, modifyParams, modulePath, ModuleResult(..), MonadClean(getParams), Params(moduVerse, sourceDirs, testMode), parseFileWithComments, runMonadClean)
import Language.Haskell.Modules.Params (modifyModuVerse, modifyTestMode)
import Language.Haskell.Modules.Split (splitModule)
import Language.Haskell.Modules.Util.QIO (noisily)
import Language.Haskell.Modules.Util.Symbols (exports, imports, symbols)
import Language.Haskell.Modules.Util.Test (diff, repoModules)
import Prelude hiding (writeFile)
import System.Cmd (system)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((<.>))
import Test.HUnit (assertEqual, Test(TestCase, TestList, TestLabel))

tests :: Test
tests = TestList [split1, split2a, split2b, split4]

split1 :: Test
split1 =
    TestCase $
      do _ <- system "rsync -aHxS --delete testdata/debian/ tmp"
         runMonadClean $ noisily $ noisily $
           do modifyParams (\ p -> p {sourceDirs = ["tmp"], moduVerse = Just repoModules})
              splitModule (S.ModuleName "Debian.Repo.Package")
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
            splitModule (S.ModuleName "Split")
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
            splitModule (S.ModuleName "Split")
       (code, out, err) <- diff "testdata/split2-clean-expected" "tmp"
       -- The output of splitModule is "correct", but it will not be
       -- accepted by GHC until the fix for
       -- http://hackage.haskell.org/trac/ghc/ticket/8011 is
       -- available.
       assertEqual "split2-clean" (ExitFailure 1,"diff -ru '--exclude=*~' '--exclude=*.imports' testdata/split2-clean-expected/Split/Clean.hs tmp/Split/Clean.hs\n--- testdata/split2-clean-expected/Split/Clean.hs\n+++ tmp/Split/Clean.hs\n@@ -6,7 +6,7 @@\n \n \n import Data.Char (isAlphaNum)\n-import URL (ToURL(toURL), URLT)\n+import URL (ToURL(URLT, toURL))\n \n clean :: (ToURL url, Show (URLT url)) => url -> String\n clean = filter isAlphaNum . show . toURL\n", "") (code, out, err)

split4 :: Test
split4 =
    TestLabel "Split4" $ TestCase $
    do system "rsync -aHxs --delete testdata/split4/ tmp"
       withCurrentDirectory "tmp" $
         runMonadClean $ modifyTestMode (const True) >> modifyModuVerse (const Set.empty) >> splitModule (S.ModuleName "Split4")
       result <- diff "testdata/split4-expected" "tmp"
       assertEqual "Split4" (ExitSuccess, "", "") result
