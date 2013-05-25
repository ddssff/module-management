{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Cat
    ( catModules
    ) where

import Debug.Trace

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try, throw)
import Control.Monad as List (mapM, mapM_, filterM)
import Control.Monad.Trans (liftIO)
import Data.Char (isAlpha)
import Data.Generics (Data, mkT, everywhere)
import Data.List as List (findIndex, tails, null, partition, nub, map)
import Data.Map as Map (Map, lookup, fromList, elems, keys)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (Set, toList, fromList, difference, member, insert, union, null, map)
import Data.Set.Extra as Set (mapM, mapM_)
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(ParseOk, ParseFailed))
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpan)
import Language.Haskell.Exts.Syntax (ImportDecl(ImportDecl, importModule, importSpecs), ImportSpec, Module(Module), ModuleName(..), QName(Qual), Name(..), ExportSpec(..), ImportSpec(..), Decl)
import Language.Haskell.Imports.Clean (cleanImports)
import Language.Haskell.Imports.Common (replaceFileIfDifferent, tildeBackup, withCurrentDirectory, removeFileIfPresent, modulePath, checkParse)
import Language.Haskell.Imports.Fold (foldModule)
import Language.Haskell.Imports.Params (dryRun, MonadParams, putDryRun, runParamsT)
import Language.Haskell.Imports.Syntax (importsSpan, renameSpec, replaceImports)
import System.Cmd (system)
import System.FilePath ((<.>))
import Test.HUnit (Test(TestCase), assertEqual)

-- | Merge the declarations from several modules into a single new
-- one.  Note that a circular imports can be created by this
-- operation, in which case you will have to add more modules to the
-- merge.
catModules :: Set ModuleName -> [ModuleName] -> ModuleName -> IO ()
catModules all from to
    | List.null from = throw $ userError "catModules: invalid argument"
    | elem to from = throw $ userError "catModules: invalid destination"
    | True = do let all' = union all (Set.fromList from)
                from' <- List.mapM (\ name -> do text <- readFile (modulePath name)
                                                 (m, comments) <- liftIO (checkParse name <$> parseFileWithComments defaultParseMode (modulePath name))
                                                 return (name, m, comments, text)) from
                -- Generate the modified modules
                changed <- filterM (catModules' all' from' to) (Set.toList all') >>=
                           -- The first from module turned into the to
                           -- module, the other from modules disappeared.
                           return . Set.map (\ x -> if elem x from then to else x) . Set.fromList
                -- Remove the original modules
                List.mapM_ (removeFileIfPresent . modulePath) from
                -- Clean up the imports of the new modules
                runParamsT $ List.mapM_ cleanImports (List.map modulePath (Set.toList changed))

-- | Update the module 'name' to reflect the result of the cat operation.
catModules' :: Set ModuleName -> [(ModuleName, Module, [Comment], String)] -> ModuleName -> ModuleName -> IO Bool
catModules' all from@((first, _, _, _) : _) to name =
    do text <- liftIO . readFile . modulePath $ name
       (m@(Module loc _ p w e i d), comments) <- liftIO (checkParse name <$> parseFileWithComments defaultParseMode (modulePath name))
       let name' = if name == first then to else name
           text' = catModules'' all from to (name, m, comments, text)
       replaceFileIfDifferent (modulePath name') text'

catModules'' :: Set ModuleName -> [(ModuleName, Module, [Comment], String)] -> ModuleName -> (ModuleName, Module, [Comment], String) -> String
catModules'' all from@((first, _, _, _) : _) to (name, m, comments, text) =    foldModule headf importf declf tailf m comments text "" ++ "\n"
    where
      from' = List.map (\ (x, _, _, _) -> x) from
      headf (Module l name p w e i d) pre s sp r
          | name == first =
              -- Change the module name of "first" to "to".  Add exports from the other "from" modules
              r <>
              maybe "" fst pre <> prettyPrintWithMode defaultMode (Module l to p w (mergeExports from to) [] []) <> "\n\n" <>
              concatMap (imports from') from <> "\n\n" <>
              concatMap decls from
          | not (elem name (to : from')) =
              -- Just modify the EModuleContents exports
              let e' = maybe Nothing (Just . updateModuleContentsExports from' to) e in
              r <> maybe "" fst pre <> if e /= e' then prettyPrintWithMode defaultMode (Module l name  p w e' [] []) <> "\n" else s
          | True =
              -- These are going to be deleted
              r
      importf x pre s sp r
          | name == first =
              -- Imports of "from" modules need to be removed
              r -- if elem (importModule x) from' then r else r <> t3 (maybe "" fst pre <> s)
          | not (elem name (to : from')) =
              -- Imports of "from" modules need to be changed to "to" module
              let x' = x {importModule = if elem (importModule x) from' then to else importModule x} in
              r <> maybe "" fst pre <> if x /= x' then prettyPrintWithMode defaultMode x' <> "\n" else s
          | True = r <> maybe "" fst pre <> s -- No changes required
      declf x pre s sp r
          | name == first =
              -- Keep original declarations, add declarations from other "from" modules
              r -- <> maybe "" fst pre <> s
          | not (elem name (to : from')) = r <> maybe "" fst pre <> s -- No changes
          | True = r -- No output
      tailf s sp r = r ++ s

t1 x = trace ("New exports: " ++ show x) x

mergeExports :: [(ModuleName, Module, [Comment], String)] -> ModuleName -> Maybe [ExportSpec]
mergeExports from to =
    Just (t1 (concatMap mergeExports' from))
    where
      mergeExports' (_, Module _ _ _ _ Nothing _ _, _, _) = error "catModules: no explicit export list"
      mergeExports' (_, Module _ _ _ _ (Just e) _ _, _, _) = updateModuleContentsExports from' to e
      from' = List.map (\ (x, _, _, _) -> x) from

updateModuleContentsExports :: [ModuleName] -> ModuleName -> [ExportSpec] -> [ExportSpec]
updateModuleContentsExports from to es =
    foldl f [] es
    where
      f ys e@(EModuleContents m) = let e' = EModuleContents (if elem m from then to else m) in
                                   if elem e' ys then ys else e' : ys
      f ys e = e : ys

imports :: [ModuleName] -> (ModuleName, Module, [Comment], String) -> String
imports from' (_, m, comments, text) =
    foldModule (\ _ _ _ _ r -> r) (\ x pre s _ r -> r <> maybe "" fst pre <> if elem (importModule x) from' then "" else s) (\ _ _ _ _ r -> r) (\ s _ r -> r <> s)
               m comments text "" <> "\n"

decls :: (ModuleName, Module, [Comment], String) -> String
decls (_, m, comments, text) =
    foldModule (\ _ _ _ _ r -> r) (\ _ _ _ _ r -> r) (\ _ pre s _ r -> r <> maybe "" fst pre <> s) (\ s _ r -> r <> s)
               m comments text "" <> "\n\n"

fixImports :: Data a => Set ModuleName -> ModuleName -> a -> a
fixImports from to x =
    everywhere (mkT moveModuleName) x
    where
      moveModuleName :: ModuleName -> ModuleName
      moveModuleName name = if member name from then to else name

test1 :: Test
test1 =
    TestCase
      (system "rsync -aHxS --delete testdata/ testcopy" >>
       withCurrentDirectory "testcopy"
         (catModules
           (Set.fromList testModules)
           [(ModuleName "Debian.Repo.AptCache"), (ModuleName "Debian.Repo.AptImage")]
           (ModuleName "Debian.Repo.Cache") >>= \ () ->
            assertEqual
              "catModules"
              ()
              ()))

test2 :: Test
test2 =
    TestCase
      (system "rsync -aHxS --delete testdata/ testcopy" >>
       withCurrentDirectory "testcopy"
         (catModules
           (Set.fromList testModules)
           [ModuleName "Debian.Repo.Types.Slice", ModuleName "Debian.Repo.Types.Repo", ModuleName "Debian.Repo.Types.EnvPath"]
           (ModuleName "Debian.Repo.Types.Common") >>= \ () ->
            assertEqual
              "catModules"
              ()
              ()))

testModules =
            [ModuleName "Debian.Repo.Sync",
             ModuleName "Debian.Repo.Slice",
             ModuleName "Debian.Repo.SourcesList",
             ModuleName "Debian.Repo.PackageIndex",
             ModuleName "Debian.Repo.Types.Slice",
             ModuleName "Debian.Repo.Types.Repository",
             ModuleName "Debian.Repo.Types.PackageIndex",
             ModuleName "Debian.Repo.Types.Release",
             ModuleName "Debian.Repo.Types.AptImage",
             ModuleName "Debian.Repo.Types.Repo",
             ModuleName "Debian.Repo.Types.AptBuildCache",
             ModuleName "Debian.Repo.Types.EnvPath",
             ModuleName "Debian.Repo.Types.AptCache",
             ModuleName "Debian.Repo.Orphans",
             ModuleName "Debian.Repo.Types",
             ModuleName "Debian.Repo.AptImage",
             ModuleName "Debian.Repo.Package",
             ModuleName "Debian.Repo.Monads.Top",
             ModuleName "Debian.Repo.Monads.Apt",
             ModuleName "Debian.Repo.AptCache",
             ModuleName "Tmp.File",
             ModuleName "Text.Format"]
