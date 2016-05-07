{-# LANGUAGE RankNTypes #-}
module Split where

import Control.Monad as List (mapM_)
import Data.Monoid ((<>))
import qualified Language.Haskell.Exts.Annotated.Syntax as A -- (Decl)
import Language.Haskell.Exts.Annotated.Simplify
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..), Name(Ident))
import Language.Haskell.Modules.ModuVerse (CleanMode(DoClean))
import Language.Haskell.Modules (noisily, putHsSourceDirs, putModule, runModuVerseT, splitModule, splitModuleBy, withCurrentDirectory, findHsModules, extraImport)
import Language.Haskell.Modules.Split (T(..))
import Language.Haskell.Modules.Symbols (foldDeclared)
import Language.Haskell.Modules.Util.Test (diff, repoModules, rsync)
import Prelude hiding (writeFile)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath (FilePath, (</>))
import Test.HUnit -- (assertEqual, Test(TestCase, TestList, TestLabel))
--import Debug.Trace

tests :: Test
tests = TestList [split1, {-split2a, split2b, split4,-} split4b, split4c, {-split5,-} split6, split7 {-,split7b0-}]

tmp :: FilePath
tmp = "tests/tmp"

indent :: String -> String -> String
indent pre s = unlines $ map (pre ++) $ lines s

split1 :: Test
split1 =
    TestLabel "split1" $ TestCase $
      do _ <- rsync "tests/data/split1" tmp
         _ <- runModuVerseT $ noisily $ noisily $
           do putHsSourceDirs [tmp]
              mapM_ putModule [S.ModuleName "Split1"]
              splitModule DoClean f (tmp </> "Split1.hs")
         (code, out, err) <- diff "tests/data/split1-expected" tmp
         assertString (indent "  " $
                       (if code == ExitSuccess then "" else "Unexpected exit code: " ++ show code ++ "\n") ++
                       (if null out then "" else "Unexpected differences:\n" ++ indent "  " out) ++
                       (if null err then "" else "Unexpected error output:\n" ++ indent "  " err))
    where
      f :: S.ModuleName -> T -> S.ModuleName
      -- Matches an instance of class "Ppr"
      f modName (A x@(A.InstDecl _ _ (A.IRule _ _ _ (A.IHApp _ (A.IHCon _ (A.UnQual _ (A.Ident _ "Ppr"))) _)) _)) = {-trace (show (sDecl x))-} (S.ModuleName "Split2")
      f modName@(S.ModuleName "Split1") (A decl) =
          foldDeclared (\symName r -> if elem symName (map S.Ident ["pprint1", "pprintW", "pprintL", "pprintStyle", "friendlyNames"]) then S.ModuleName "Split2" else r) modName decl
      f modName _ = modName

{-
split2a :: Test
split2a =
    TestCase $
    do _ <- rsync "tests/data/split2" tmp
       _ <- runModuVerseT $ noisily $ noisily $
         do putHsSourceDirs [tmp]
            putModule (S.ModuleName "Split")
            splitModuleDecls DoClean (tmp </> "Split.hs")
       (code, out, err) <- diff "tests/data/split2-expected" tmp
       assertEqual "split2a" (ExitSuccess, "", "") (code, out, err)

split2b :: Test
split2b =
    TestCase $
    do _ <- rsync "tests/data/split2" tmp
       _ <- runModuVerseT $ noisily $ noisily $
         do putHsSourceDirs [tmp]
            putModule (S.ModuleName "Split")
            splitModuleDecls DoClean (tmp </> "Split.hs")
       (code, out, err) <- diff "tests/data/split2-clean-expected" tmp
       -- The output of splitModule is "correct", but it will not be
       -- accepted by GHC until the fix for
       -- http://hackage.haskell.org/trac/ghc/ticket/8011 is
       -- available.
       assertEqual "split2b" (ExitFailure 1,"diff -ru '--exclude=*~' '--exclude=*.imports' tests/data/split2-clean-expected/Split/Clean.hs tmp/Split/Clean.hs\n--- tests/data/split2-clean-expected/Split/Clean.hs\n+++ tmp/Split/Clean.hs\n@@ -6,7 +6,7 @@\n     ) where\n \n import Data.Char (isAlphaNum)\n-import URL (ToURL(toURL), URLT)\n+import URL (ToURL(URLT, toURL))\n \n clean :: (ToURL url, Show (URLT url)) => url -> String\n clean = filter isAlphaNum . show . toURL\n", "") (code, out, err)

split4 :: Test
split4 =
    TestLabel "Split4" $ TestCase $
    do _ <- rsync "tests/data/split4" tmp
       _ <- withCurrentDirectory tmp $
         runModuVerseT $ noisily $ noisily $
           putModule (S.ModuleName "Split4") >>
           splitModuleDecls DoClean "Split4.hs"
       result <- diff "tests/data/split4-expected" tmp
       assertEqual "split4" (ExitSuccess, "", "") result
-}

split4b :: Test
split4b =
    TestLabel "Split4b" $ TestCase $
    do _ <- rsync "tests/data/split4" tmp
       _ <- withCurrentDirectory tmp $
         runModuVerseT $ noisily $ noisily $
           putModule (S.ModuleName "Split4") >>
           splitModule DoClean f "Split4.hs"
       result <- diff "tests/data/split4b-expected" tmp
       assertEqual "split4b" (ExitSuccess, "", "") result
    where
      f :: forall t. S.ModuleName -> T -> S.ModuleName
      f modName (A decl) = foldDeclared (\name r -> if elem name (map S.Ident ["getPackages", "sourcePackagesOfIndex", "binaryPackagesOfIndex"])
                                                    then S.ModuleName "A"
                                                    else r) (S.ModuleName "B") decl
      f _ _ = S.ModuleName "Split4.B"


split4c :: Test
split4c =
    TestLabel "Split4b" $ TestCase $
    do _ <- rsync "tests/data/split4" tmp
       _ <- withCurrentDirectory tmp $
         runModuVerseT $ noisily $ noisily $
           putModule (S.ModuleName "Split4") >>
           splitModule DoClean f "Split4.hs"
       result <- diff "tests/data/split4c-expected" tmp
       assertEqual "split4c" (ExitSuccess, "", "") result
    where
      f :: S.ModuleName -> T -> S.ModuleName
      f modName (A decl) = foldDeclared (\name r -> if name == S.Ident "getPackages" then S.ModuleName "Split4.A" else r) modName decl
      f modName _ = modName

{-
-- Test what happens when a split module is re-exported
split5 :: Test
split5 =
    TestLabel "Split5" $ TestCase $
    do _ <- rsync "tests/data/split5" tmp
       _ <- withCurrentDirectory tmp $
         runModuVerseT $ noisily $ noisily $
           List.mapM_ (putModule . S.ModuleName) ["A", "B", "C", "D", "E"] >>
           splitModuleDecls DoClean "B.hs"
       result <- diff "tests/data/split5-expected" tmp
       assertEqual "split5" (ExitSuccess, "", "") result
-}

split6 :: Test
split6 =
    TestLabel "Split6" $ TestCase $
    do _ <- rsync "tests/data/debian" tmp
       _ <- withCurrentDirectory tmp $
         findHsModules ["Debian", "Text", tmp] >>= \ modules ->
         runModuVerseT $ noisily $ noisily $
           mapM putModule modules >>
           splitModule DoClean f "Debian/Repo/Monads/Apt.hs"
       result <- diff "tests/data/split6-expected" tmp
       assertEqual "split6" (ExitSuccess, "", "") result
    where
      f :: S.ModuleName -> T -> S.ModuleName
      f modName (A decl) = foldDeclared (\name r -> if name == S.Ident "countTasks" then S.ModuleName "IO" else r) modName decl
      f _ _ = S.ModuleName "Debian.Repo.Monads.Apt"

-- This code is, in some sense, inherently unsplittable.
split7 :: Test
split7 =
    TestLabel "split7" $ TestCase $
    do _ <- rsync "tests/data/fold3b" tmp
       _ <- withCurrentDirectory tmp $ runModuVerseT $
            do putModule (S.ModuleName "Main")
               extraImport (S.ModuleName "Main.GetPasteById") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.GetRecentPastes") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.InitialCtrlVState") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.InsertPaste") (S.ModuleName "Main.Instances")
               splitModule DoClean f "Main.hs"
       result <- diff "tests/data/split7-expected" tmp
       assertEqual "split7" (ExitSuccess, "", "") result
    where
      f :: S.ModuleName -> T -> S.ModuleName
      f modName (A decl) = foldDeclared g modName decl
      f _ _ = S.ModuleName "Other"
      g (S.Ident "appTemplate") _ = S.ModuleName "Route"
      g (S.Ident "Route") _ = S.ModuleName "Route"
      g (S.Ident "route") _ = S.ModuleName "Route"
      g (S.Ident "CtrlV") _ = S.ModuleName "Route"
      g (S.Ident "CtrlV'") _ = S.ModuleName "Route"
      g (S.Ident "CtrlVForm") _ = S.ModuleName "Route"
      g (S.Ident "CtrlVState") _ = S.ModuleName "Route"
      g (S.Ident "viewPastePage") _ = S.ModuleName "Route"
      g (S.Ident "viewRecentPage") _ = S.ModuleName "Route"
      g (S.Ident "newPastePage") _ = S.ModuleName "Route"
      g (S.Ident "main") _ = S.ModuleName "Main"
      g (S.Ident "PasteId") _ = S.ModuleName "PasteId"
      g (S.Ident "Format") _ = S.ModuleName "Format"
      g (S.Ident "PasteMeta") _ = S.ModuleName "PasteMeta"
      g (S.Ident "Paste") _ = S.ModuleName "Paste"
      g (S.Ident "initialCtrlVState,") _ = S.ModuleName "InitialCtrlVState"
      g (S.Ident "insertPaste") _ = S.ModuleName "InsertPaste"
      g (S.Ident "getPasteById") _ = S.ModuleName "GetPasteById"
      g (S.Ident "Limit") _ = S.ModuleName "Limit"
      g (S.Ident "Offset") _ = S.ModuleName "Offset"
      g (S.Ident "getRecentPastes") _ = S.ModuleName "GetRecentPastes"
      g (S.Ident "formatPaste") _ = S.ModuleName "FormatPaste"
      g (S.Ident "pasteForm") _ = S.ModuleName "PasteForm"
      g _ r = r

{-
split7b :: Test
split7b =
    TestLabel "split7b" $ TestCase $
    do _ <- rsync "tests/data/fold3b" tmp
       _ <- withCurrentDirectory tmp $ runModuVerseT $
            do putModule (S.ModuleName "Main")
               extraImport (S.ModuleName "Main.GetPasteById") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.GetRecentPastes") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.InitialCtrlVState") (S.ModuleName "Main.Instances")
               extraImport (S.ModuleName "Main.InsertPaste") (S.ModuleName "Main.Instances")
               splitModuleDecls DoClean "Main.hs"
       result <- diff "tests/data/split7-expected" tmp
       assertEqual "split7b" (ExitSuccess, "", "") result
-}
