{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Cat
    ( catModules
    , test1
    , test2
    , test3
    ) where

import Control.Applicative ((<$>))
import Control.Exception (throw)
import Control.Monad as List (mapM_)
import Control.Monad.Trans (liftIO)
import Data.Default (def)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.List as List (filter, intercalate, isPrefixOf, map)
import Data.Map as Map (Map, fromList, member, toAscList, lookup)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (Monoid, (<>))
import Data.Set as Set (Set, fromList, toList, union)
import Language.Haskell.Exts.Annotated (defaultParseMode, parseFileWithComments)
import Language.Haskell.Exts.Annotated.Simplify (sDecl, sExportSpec, sModuleName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ExportSpecList(ExportSpecList), ImportDecl(importModule), Module(Module), ModuleHead(ModuleHead), ModuleName(ModuleName))
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(EModuleContents), ModuleName(..))
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrintWithMode)
import Language.Haskell.Modules.Common (checkParse, Module, modulePath, removeFileIfPresent, replaceFile, tildeBackup, withCurrentDirectory)
import Language.Haskell.Modules.Fold (foldModule)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Params (MonadClean, runCleanT)
import System.Cmd (system)
import System.Exit (ExitCode(ExitFailure))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual, Test(TestCase))

data Result
    = Unchanged
    | Removed S.ModuleName
    | Modified S.ModuleName String

-- | Merge the declarations from several modules into a single new
-- one.  Note that a circular imports can be created by this
-- operation, in which case you will have to add more modules to the
-- merge.
catModules :: MonadClean m => Set S.ModuleName -> [S.ModuleName] -> S.ModuleName -> m ()
catModules univ inputs output =
    liftIO (catModulesIO univ inputs output) >>= List.mapM_ cleanImports . List.map modulePath

catModulesIO :: Set S.ModuleName -> [S.ModuleName] -> S.ModuleName -> IO [S.ModuleName]
catModulesIO _ [] _ = throw $ userError "catModules: invalid argument"
catModulesIO univ inputs output =
    do let univ' = union univ (Set.fromList (output : inputs))
       info <- loadModules inputs
       mapM (doModule info inputs output) (Set.toList univ') >>= mapM doResult >>= return . catMaybes
       {- >>= return . Set.map (\ x -> if elem x inputs then output else x) . Set.fromList -}
       -- List.mapM_ (removeFileIfPresent . modulePath) inputs
       -- return changed
    where
      doResult :: Result -> IO (Maybe S.ModuleName)
      doResult Unchanged = return Nothing
      doResult (Removed name) = removeFileIfPresent (modulePath name) >> return (Just name)
      doResult (Modified name text) = replaceFile tildeBackup (modulePath name) text >> return (Just name)

-- Update the module 'name' to reflect the result of the cat operation.
doModule :: Map S.ModuleName (Module, String) -> [S.ModuleName] -> S.ModuleName -> S.ModuleName -> IO Result
doModule info input@(first : _) output name =
    do -- The new module will be based on the existing module, unless
       -- name equals output and output does not exist
       let base = if name == output && not (Map.member name info) then first else name
       (m, text) <- maybe (loadModule base) return (Map.lookup name info)
       let text' = doModule' info input output (name, (m, text))
       return $ if name == output
                then Modified name text'
                else if elem name input
                     then Removed name
                     else if text /= text'
                          then Modified name text'
                          else Unchanged
       -- if base /= name || text' /= text then (replaceFile (const Nothing) (modulePath name) text' >> return True) else return False
       -- replaceFileIfDifferent (modulePath name) text'

doModule' :: Map S.ModuleName (Module, String) -> [S.ModuleName] -> S.ModuleName -> (S.ModuleName, (Module, String)) -> String
doModule' _ [] _ _ = error "doModule'"
doModule' info inputs output (name, (m, text)) =
    case () of
      _ | name == output -> doOutput info inputs output (m, text)
        | not (elem name (output : inputs)) -> doOther inputs output (m, text)
        | True -> ""

doOutput :: Map S.ModuleName (Module, String) -> [S.ModuleName] -> S.ModuleName -> (Module, String) -> String
doOutput info inputs output (m, text) =
    header ++ fromMaybe "" exports ++ fromMaybe "" imports ++ fromMaybe "" decls
    where
      header =
          foldModule echo
                     (\ _ pre _ r -> r <> pre <> prettyPrintWithMode defaultMode output)
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
                     (\ _e pre _ r -> Just (fromMaybe (pre <> maybe "" (intercalate ", " . List.map (prettyPrintWithMode defaultMode)) (mergeExports info output)) r))
                     ignore
                     ignore
                     (\ _ r -> r)
                     m text Nothing
      imports =
          foldModule ignore
                     ignore
                     ignore
                     ignore
                     (\ _i pre _ r -> Just (fromMaybe (pre <> unlines (List.map (moduleImports info) inputs)) r))
                     ignore
                     (\ _ r -> r)
                     m text Nothing
      decls =
          foldModule ignore
                     ignore
                     ignore
                     ignore
                     ignore
                     (\ _d _ _ r -> Just (fromMaybe (unlines (List.map (moduleDecls info output) inputs)) r))
                     (\ s r -> Just (maybe s (<> s) r))
                     m text Nothing

echo :: Monoid m => t -> m -> m -> m -> m
echo _ pre s r = r <> pre <> s

ignore :: t -> t1 -> t2 -> t3 -> t3
ignore _ _ _ r = r

doOther :: [S.ModuleName] -> S.ModuleName -> (Module, String) -> String
doOther inputs input@(S.ModuleName input') (m, text) =
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
                          r <> pre <> (if elem (sModuleName (A.importModule x)) inputs
                                       then prettyPrintWithMode defaultMode (x {A.importModule = A.ModuleName def input'})
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
                     | elem y inputs ->
                         "\n     , " <> prettyPrintWithMode defaultMode (S.EModuleContents input)
                 _ -> pre <> s

mergeExports :: Map S.ModuleName (Module, String) -> S.ModuleName -> Maybe [S.ExportSpec]
mergeExports old new =
    Just (concatMap mergeExports' (Map.toAscList old))
    where
      mergeExports' (_, (A.Module _ Nothing _ _ _, _)) = error "catModules: no explicit export list"
      mergeExports' (_, (A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _, _)) = error "catModules: no explicit export list"
      mergeExports' (_, (A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList _ e)))) _ _ _, _)) = updateModuleContentsExports old new (List.map sExportSpec e)
      mergeExports' (_, _) = error "mergeExports'"

updateModuleContentsExports :: Map S.ModuleName (Module, String) -> S.ModuleName -> [S.ExportSpec] -> [S.ExportSpec]
updateModuleContentsExports old new es =
    foldl f [] es
    where
      f :: [S.ExportSpec] -> S.ExportSpec ->  [S.ExportSpec]
      f ys (S.EModuleContents m) =
          let e' = S.EModuleContents (if Map.member m old then new else m) in
          ys ++ if elem e' ys then [] else [e']
      f ys e = ys ++ [e]

moduleImports :: Map S.ModuleName (Module, String) -> S.ModuleName -> String
moduleImports old name =
    let (Just (m, text)) = Map.lookup name old in
    foldModule (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ x pre s r ->
                    r
                    -- If this is the first import, omit the prefix, it includes the ") where" text.
                    <> (if r == "" then "" else pre)
                    <> if Map.member (sModuleName (A.importModule x)) old then "" else s)
               (\ _ _ _ r -> r)
               (\ _ r -> r)
               m text "" <> "\n"

-- | Grab the declarations out of the old modules, fix any
-- qualified symbol references, prettyprint and return.
moduleDecls :: Map S.ModuleName (Module, String) -> S.ModuleName -> S.ModuleName -> String
moduleDecls old new name =
    let (Just (m, text)) = Map.lookup name old in
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
fixReferences :: (Data a, Typeable a) => Map S.ModuleName (Module, String) -> S.ModuleName -> a -> a
fixReferences old new x =
    everywhere (mkT moveModuleName) x
    where
      moveModuleName :: S.ModuleName -> S.ModuleName
      moveModuleName name@(S.ModuleName _) = if Map.member name old then new else name

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

test3 :: Test
test3 =
    TestCase
      (system "rsync -aHxS --delete testdata/original/ testdata/copy" >>
       withCurrentDirectory "testdata/copy"
         (runCleanT "dist/scratch"
          (catModules
           (Set.fromList testModules)
           [S.ModuleName "Debian.Repo.Types.Slice", S.ModuleName "Debian.Repo.Types.Repo", S.ModuleName "Debian.Repo.Types.EnvPath"]
           (S.ModuleName "Debian.Repo.Types.Repo")) >>
          mapM_ removeFileIfPresent junk) >>
       readProcessWithExitCode "diff" ["-ru", "--unidirectional-new-file", "testdata/catresult", "testdata/copy"] "" >>= \ (code, out, err) ->
       let out' = unlines (List.filter (not . isPrefixOf "Binary files") . List.map (takeWhile (/= '\t')) $ (lines out)) in
       assertEqual "catModules" (ExitFailure 2, "", "") (code, out', err))

junk :: [String]
junk =
    [ "Debian.Repo.Monads.Apt.imports"
    , "Debian.Repo.Monads.Top.imports"
    , "Debian.Repo.Orphans.imports"
    , "Debian.Repo.PackageIndex.imports"
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

loadModule :: S.ModuleName -> IO (Module, String)
loadModule name =
    do text <- readFile (modulePath name)
       (m, _) <- checkParse name <$> parseFileWithComments defaultParseMode (modulePath name)
       return (m, text)

loadModules :: [S.ModuleName] -> IO (Map S.ModuleName (Module, String))
loadModules names = mapM loadModule names >>= return . Map.fromList . zip names
