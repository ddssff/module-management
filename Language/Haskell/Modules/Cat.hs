{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Cat
    ( catModules
    , test1
    , test2
    ) where

import Control.Applicative ((<$>))
import Control.Exception (throw)
import Control.Monad as List (filterM, mapM, mapM_)
import Control.Monad.Trans (liftIO)
import Data.Default (def)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.List as List (filter, intercalate, isPrefixOf, map, null)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, (<>))
import Data.Set as Set (fromList, map, member, Set, toList, union)
import Language.Haskell.Exts.Annotated (defaultParseMode, parseFileWithComments)
import Language.Haskell.Exts.Annotated.Simplify (sDecl, sExportSpec, sModuleName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ExportSpecList(ExportSpecList), ImportDecl(importModule), Module(Module), ModuleHead(ModuleHead), ModuleName(ModuleName))
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(EModuleContents), ModuleName(..))
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrintWithMode)
import Language.Haskell.Modules.Common (checkParse, Module, modulePath, removeFileIfPresent, replaceFileIfDifferent, withCurrentDirectory)
import Language.Haskell.Modules.Fold (foldModule)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Params (MonadClean, runCleanT)
import System.Cmd (system)
import System.Exit (ExitCode(ExitFailure))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual, Test(TestCase))

-- | Merge the declarations from several modules into a single new
-- one.  Note that a circular imports can be created by this
-- operation, in which case you will have to add more modules to the
-- merge.
catModules :: MonadClean m => Set S.ModuleName -> [S.ModuleName] -> S.ModuleName -> m ()
catModules univ old new = liftIO (catModulesIO univ old new) >>= List.mapM_ cleanImports . List.map modulePath . Set.toList

catModulesIO :: Set S.ModuleName -> [S.ModuleName] -> S.ModuleName -> IO (Set S.ModuleName)
catModulesIO univ old new
    | List.null old = throw $ userError "catModules: invalid argument"
    | elem new old = throw $ userError "catModules: invalid destination"
    | True = do let univ' = union univ (Set.fromList old)
                old' <- List.mapM (\ name -> do text <- readFile (modulePath name)
                                                (m, _) <- checkParse name <$> parseFileWithComments defaultParseMode (modulePath name)
                                                return (name, m, text)) old
                -- Generate the modified modules
                changed <- filterM (doModule old' new) (Set.toList univ') >>=
                           -- The first from module turned into the new
                           -- module, the other from modules disappeared.
                           return . Set.map (\ x -> if elem x old then new else x) . Set.fromList
                -- Remove the original modules
                List.mapM_ (removeFileIfPresent . modulePath) old
                return changed

-- Update the module 'name' to reflect the result of the cat operation.
doModule :: [(S.ModuleName, Module, String)] -> S.ModuleName -> S.ModuleName -> IO Bool
doModule old@((first, _, _) : _ ) new name =
          do text <- readFile . modulePath $ name
             (m, _) <- checkParse name <$> parseFileWithComments defaultParseMode (modulePath name)
             let name' = if name == first then new else name
                 text' = catModules'' old new (name, m, text)
             replaceFileIfDifferent (modulePath name') text'
doModule [] _ _ = error "catModulesIO"

catModules'' :: [(S.ModuleName, Module, String)] -> S.ModuleName -> (S.ModuleName, Module, String) -> String
catModules'' [] _ _ = error "catModules''"
catModules'' from@((first, _, _) : _) to (name, m, text) =
    case () of
      _ | name == first -> doFirst from to (m, text)
        | not (elem name (to : fromNames)) -> doOther from to (m, text)
        | True -> ""
    where
      fromNames = List.map (\ (x, _, _) -> x) from

doFirst :: [(S.ModuleName, Module, String)] -> S.ModuleName -> (Module, String) -> String
doFirst old new (m, text) =
    header ++ fromMaybe "" exports ++ fromMaybe "" imports ++ fromMaybe "" decls
    where
      header =
          foldModule echo
                     (\ _ pre _ r -> r <> pre <> prettyPrintWithMode defaultMode new)
                     echo
                     ignore
                     ignore
                     ignore
                     (\ _ r -> r)
                     m text ""
      exports =
          foldModule ignore
                     ignore
                     ignore
                     (\ _e pre _ r -> Just (fromMaybe (pre <> maybe "" (intercalate ", " . List.map (prettyPrintWithMode defaultMode)) (mergeExports old new)) r))
                     ignore
                     ignore
                     (\ _ r -> r)
                     m text Nothing
      imports =
          foldModule ignore
                     ignore
                     ignore
                     ignore
                     (\ _i pre _ r -> Just (fromMaybe (pre <> unlines (List.map (moduleImports oldNames) old)) r))
                     ignore
                     (\ _ r -> r)
                     m text Nothing
      decls =
          foldModule ignore
                     ignore
                     ignore
                     ignore
                     ignore
                     (\ _d _ _ r -> Just (fromMaybe (unlines (List.map (moduleDecls (fromList oldNames) new) old)) r))
                     (\ s r -> Just (maybe s (<> s) r))
                     m text Nothing

      oldNames = List.map (\ (x, _, _) -> x) old

echo :: Monoid m => t -> m -> m -> m -> m
echo _ pre s r = r <> pre <> s

ignore :: t -> t1 -> t2 -> t3 -> t3
ignore _ _ _ r = r

doOther :: [(S.ModuleName, Module, String)] -> S.ModuleName -> (Module, String) -> String
doOther old new@(S.ModuleName new') (m, text) =
    header ++ exports ++ imports ++ decls
    where
      header = foldModule echo echo echo ignore ignore ignore (\ _ r -> r) m text ""
      exports = foldModule ignore ignore ignore fixModuleExport ignore ignore (\ _ r -> r) m text ""
      imports =
          foldModule ignore
                     ignore
                     ignore
                     ignore
                     (\ x pre s r ->
                          r <> pre <> (if elem (sModuleName (A.importModule x)) oldNames
                                       then prettyPrintWithMode defaultMode (x {A.importModule = A.ModuleName def new'})
                                       else s))
                     ignore
                     (\ _ r -> r)
                     m text ""
      decls =
          foldModule ignore
                     ignore
                     ignore
                     ignore
                     ignore
                     echo
                     (\ s r -> r <> s)
                     m text ""

      fixModuleExport x pre s r =
          r <> case sExportSpec x of
                 S.EModuleContents y
                     | elem y oldNames ->
                         "\n     , " <> prettyPrintWithMode defaultMode (S.EModuleContents new)
                 _ -> pre <> s

      oldNames = List.map (\ (x, _, _) -> x) old

mergeExports :: [(S.ModuleName, Module, String)] -> S.ModuleName -> Maybe [S.ExportSpec]
mergeExports old new =
    Just (concatMap mergeExports' old)
    where
      mergeExports' (_, A.Module _ Nothing _ _ _, _) = error "catModules: no explicit export list"
      mergeExports' (_, A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _, _) = error "catModules: no explicit export list"
      mergeExports' (_, A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList _ e)))) _ _ _, _) = updateModuleContentsExports oldNames new (List.map sExportSpec e)
      mergeExports' (_, _, _) = error "mergeExports'"
      oldNames = List.map (\ (x, _, _) -> x) old

updateModuleContentsExports :: [S.ModuleName] -> S.ModuleName -> [S.ExportSpec] -> [S.ExportSpec]
updateModuleContentsExports old new es =
    foldl f [] es
    where
      f :: [S.ExportSpec] -> S.ExportSpec ->  [S.ExportSpec]
      f ys (S.EModuleContents m) =
          let e' = S.EModuleContents (if elem m old then new else m) in
          ys ++ if elem e' ys then [] else [e']
      f ys e = ys ++ [e]

moduleImports :: [S.ModuleName] -> (S.ModuleName, Module, String) -> String
moduleImports old' (_, m, text) =
    foldModule (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ x pre s r ->
                    r
                    -- If this is the first import, omit the prefix, it includes the ") where" text.
                    <> (if r == "" then "" else pre)
                    <> if elem (sModuleName (A.importModule x)) old' then "" else s)
               (\ _ _ _ r -> r)
               (\ _ r -> r)
               m text "" <> "\n"

-- | Grab the declarations out of the old modules, fix any
-- qualified symbol references, prettyprint and return.
moduleDecls :: Set S.ModuleName -> S.ModuleName -> (S.ModuleName, Module, String) -> String
moduleDecls old new (S.ModuleName _, m, text) =
    foldModule (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ d pre s r ->
                    let d' = sDecl d
                        d'' = fixReferences old new d' in
                    r <>
                    -- Omit the first pre of each module, it probably contains ") where"
                    (if r /= "" then pre else "") <>
                    (if d'' /= d' then prettyPrintWithMode defaultMode d'' <> "\n\n" else s))
               (\ s r -> r <> s)
               m text "" <> "\n"

-- | Change any ModuleName in 'old' to 'new'.  Note that this will
-- probably mess up the location information, so the result (if
-- different from the original) should be prettyprinted, not
-- exactPrinted.
fixReferences :: (Data a, Typeable a) => Set S.ModuleName -> S.ModuleName -> a -> a
fixReferences old new x =
    everywhere (mkT moveModuleName) x
    where
      moveModuleName :: S.ModuleName -> S.ModuleName
      moveModuleName name@(S.ModuleName _) = if member name old then new else name

test1 :: Test
test1 =
    TestCase
      (system "rsync -aHxS --delete testdata/original/ testdata/copy" >>
       withCurrentDirectory "testdata/copy"
         (runCleanT "dist/scratch"
          (catModules
           (Set.fromList testModules)
           [S.ModuleName "Debian.Repo.AptCache", S.ModuleName "Debian.Repo.AptImage"]
           (S.ModuleName "Debian.Repo.Cache")) >>
            assertEqual
              "catModules"
              ()
              ()))

test2 :: Test
test2 =
    TestCase
      (system "rsync -aHxS --delete testdata/original/ testdata/copy" >>
       withCurrentDirectory "testdata/copy"
         (runCleanT "dist/scratch"
          (catModules
           (Set.fromList testModules)
           [S.ModuleName "Debian.Repo.Types.Slice", S.ModuleName "Debian.Repo.Types.Repo", S.ModuleName "Debian.Repo.Types.EnvPath"]
           (S.ModuleName "Debian.Repo.Types.Common")) >>
          mapM_ removeFileIfPresent junk) >>
       readProcessWithExitCode "diff" ["-ru", "--unidirectional-new-file", "testdata/catresult", "testdata/copy"] "" >>= \ (code, out, err) ->
       let out' = unlines (List.filter (not . isPrefixOf "Binary files") . List.map (takeWhile (/= '\t')) $ (lines out)) in
       assertEqual "catModules" (ExitFailure 2, "", "") (code, out', err))

junk :: [String]
junk =
    [ "Debian.Repo.Monads.Top.imports"
    , "Debian.Repo.Orphans.imports"
    , "Debian.Repo.SourcesList.imports"
    , "Debian.Repo.Sync.imports"
    , "Debian.Repo.Types.PackageIndex.imports"
    , "Debian.Repo.Types.Release.imports"
    , "Text.Format.imports"
    , "Tmp.File.imports"
    , "Debian/Repo/Package.hs~"
    , "Debian/Repo/PackageIndex.hs~"
    , "Debian/Repo/Types/AptBuildCache.hs~"
    , "Debian/Repo/Types/EnvPath.hs~"
    , "Debian/Repo/Types/Repo.hs~"
    , "Debian/Repo/Types/AptImage.hs~"
    , "Debian/Repo/Types/AptCache.hs~"
    , "Debian/Repo/Types/Repository.hs~"
    , "Debian/Repo/Types/Common.hs~"
    , "Debian/Repo/AptImage.hs~"
    , "Debian/Repo/Slice.hs~"
    , "Debian/Repo/AptCache.hs~"
    , "Debian/Repo/Types.hs~"
    , "Debian/Repo/Monads/Apt.hs~" ]

testModules :: [S.ModuleName]
testModules =
            [S.ModuleName "Debian.Repo.Sync",
             S.ModuleName "Debian.Repo.Slice",
             S.ModuleName "Debian.Repo.SourcesList",
             S.ModuleName "Debian.Repo.PackageIndex",
             S.ModuleName "Debian.Repo.Types",
             S.ModuleName "Debian.Repo.Types.Slice",
             S.ModuleName "Debian.Repo.Types.Repository",
             S.ModuleName "Debian.Repo.Types.PackageIndex",
             S.ModuleName "Debian.Repo.Types.Release",
             S.ModuleName "Debian.Repo.Types.AptImage",
             S.ModuleName "Debian.Repo.Types.Repo",
             S.ModuleName "Debian.Repo.Types.AptBuildCache",
             S.ModuleName "Debian.Repo.Types.EnvPath",
             S.ModuleName "Debian.Repo.Types.AptCache",
             S.ModuleName "Debian.Repo.Orphans",
             S.ModuleName "Debian.Repo.Types",
             S.ModuleName "Debian.Repo.AptImage",
             S.ModuleName "Debian.Repo.Package",
             S.ModuleName "Debian.Repo.Monads.Top",
             S.ModuleName "Debian.Repo.Monads.Apt",
             S.ModuleName "Debian.Repo.AptCache",
             S.ModuleName "Tmp.File",
             S.ModuleName "Text.Format"]
