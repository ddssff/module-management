{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Move
    ( moveModule
    , test2
    ) where

import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.Generics (mkT, everywhere)
import Data.Map as Map (Map, lookup, fromList, elems, keys)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (toList, fromList, difference)
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(ParseOk))
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpan)
import Language.Haskell.Exts.Syntax (ImportDecl, Module(Module), ModuleName(..), QName(Qual), ExportSpec(..))
import Language.Haskell.Imports.Common (replaceFileIfDifferent, withCurrentDirectory, removeFileIfPresent, modulePath)
import Language.Haskell.Imports.Fold (foldModule)
import System.Cmd (system)
import Test.HUnit (Test(TestCase), assertEqual)

-- | Modify the imports of a source file to reflect the changes
-- described in the list of FQID pairs.  There are four tasks: (1)
-- modify the name, (2) modify the imports, (3) modify qualified
-- references, and (4) move the module to the location implied by its
-- new name.
moveModule :: Map ModuleName ModuleName -> Bool -> [FilePath] -> IO ()
moveModule moves dry paths =
    mapM_ (moveModule' moves dry) paths >>
    mapM_ removeFileIfPresent (map modulePath (Set.toList (difference (Set.fromList (keys moves)) (Set.fromList (elems moves)))))

moveModule' :: Map ModuleName ModuleName -> Bool -> FilePath -> IO ()
moveModule' moves dry path =
    do ParseOk (m@(Module _ name _ _ _ _ _), comments) <- liftIO (parseFileWithComments defaultParseMode path)
       text <- liftIO $ readFile path
       let text' = foldModule headf importf declf tailf m comments text ""
           -- name' = Map.lookup name moves
       if dry then putStrLn ("replaceFile " ++ (modulePath (fromMaybe name (Map.lookup name moves)))) else void (replaceFileIfDifferent (modulePath (fromMaybe name (Map.lookup name moves))) (text' <> "\n"))
    where
      headf (Module l name p w e _i _d) pre s _sp r =
          r <> maybe "" fst pre <>
          maybe s (\ name' -> prettyPrintWithMode defaultMode (Module l name' p w (fmap (map (moveExportSpec moves)) e) [] []) <> "\n\n") (Map.lookup name moves)
      importf :: ImportDecl -> Maybe (String, SrcSpan) -> String -> SrcSpan -> String -> String
      importf x pre s _sp r =
          r <> maybe "" fst pre <>
          if x /= x'
          then prettyPrintWithMode defaultMode x' <> "\n"
          else s
          where x' = everywhere (mkT (moveModuleName moves)) x
      declf x pre s _sp r =
          r <> maybe "" fst pre <>
          if x /= x'
          then prettyPrintWithMode defaultMode x'
          else s
          where x' = everywhere (mkT (moveQName moves)) x
      tailf s _ r = r <> s

moveExportSpec :: Map ModuleName ModuleName -> ExportSpec -> ExportSpec
moveExportSpec moves x = everywhere (mkT (moveQName moves)) x

moveQName :: Map ModuleName ModuleName -> QName -> QName
moveQName moves (Qual m n) = Qual (fromMaybe m (Map.lookup m moves)) n
moveQName _ x = x

moveModuleName :: Map ModuleName ModuleName -> ModuleName -> ModuleName
moveModuleName moves m = fromMaybe m (Map.lookup m moves)

-- moveDecl :: Map ModuleName ModuleName -> Decl -> Decl
-- moveDecl moves x = everywhere (mkT (moveQName moves)) x

test2 :: Test
test2 =
    TestCase
      (system "rsync -aHxS --delete testdata/ testcopy" >>
       withCurrentDirectory "testcopy"
         (moveModule
           (Map.fromList [(ModuleName "Debian.Repo.Types.AptBuildCache", ModuleName "Debian.Repo.Types.BuildCache")])
           False
           ["Debian/Repo/Sync.hs",
            "Debian/Repo/Slice.hs",
            "Debian/Repo/SourcesList.hs",
            "Debian/Repo/PackageIndex.hs",
            "Debian/Repo/Types/Slice.hs",
            "Debian/Repo/Types/Repository.hs",
            "Debian/Repo/Types/PackageIndex.hs",
            "Debian/Repo/Types/Release.hs",
            "Debian/Repo/Types/AptImage.hs",
            "Debian/Repo/Types/Repo.hs",
            "Debian/Repo/Types/AptBuildCache.hs",
            "Debian/Repo/Types/EnvPath.hs",
            "Debian/Repo/Types/AptCache.hs",
            "Debian/Repo/Orphans.hs",
            "Debian/Repo/Types.hs",
            "Debian/Repo/AptImage.hs",
            "Debian/Repo/Package.hs",
            "Debian/Repo/Monads/Top.hs",
            "Debian/Repo/Monads/Apt.hs",
            "Debian/Repo/AptCache.hs",
            "Tmp/File.hs",
            "Text/Format.hs"] >>= \ () ->
           assertEqual
             "moveModule"
             ()
             ()))
