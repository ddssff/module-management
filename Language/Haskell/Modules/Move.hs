-- | Move declarations between modules.
{-# LANGUAGE CPP, FlexibleInstances, OverloadedLists, RankNTypes, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Move
    ( moveDeclsBy
    ) where

import Debug.Trace
import Control.Lens -- (at, use, (%=))
import Control.Monad.Trans (liftIO)
import Data.List as List (partition)
import Data.Map as Map (adjust, insert, lookup, Map, mapWithKey, toList)
import Data.Set as Set (fromList, member)
import qualified Language.Haskell.Exts.Annotated as A
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S -- (ExportSpec(..), ImportDecl(..), Module(..), ModuleName(..), Name(..))
import Language.Haskell.Modules.Fold (ModuleInfo(..))
import Language.Haskell.Modules.ModuVerse (buildSymbolMap, buildDeclMap,
                                           modulesNew, modulesOrig, ModuVerse)
import Language.Haskell.Modules.SourceDirs (PathKey)
import Language.Haskell.Modules.Symbols (foldDeclared, symbolsDeclaredBy)
import Prelude hiding (writeFile)

-- | Perform the changes described by the newModule function on all
-- the modules in the moduverse.  This includes
--    1) Removing declarations that are moving out
--    2) Removing exports of symbols whose declarations are moving out
--    3) Removing imports of symbols whose declarations are moving in
--    4) Adding imports of symbols whose declarations are moving out
--    5) Updating imports of symbols whose declartions are otherwise moving
moveDeclsBy :: forall m. (ModuVerse m) =>
               (S.ModuleName -> A.Decl SrcSpanInfo -> S.ModuleName)
            -> m ()
moveDeclsBy newModule = do
  buildSymbolMap
  buildDeclMap newModule
  mapM_ copyModule . Map.toList =<< use modulesOrig
  mapM_ (uncurry $ doModule newModule) . Map.toList =<< use modulesOrig
  writeChanges
    where
      copyModule :: (PathKey, ModuleInfo) -> m ()
      copyModule (_key, info@(ModuleInfo _ _ _ _ name)) = do
        modulesNew %= Map.insert name info

-- | Perform the modifications required for the declarations in the
-- one module.  This may change anything - declarations will be
-- removed from this module and added to other modules, and imports
-- and exports everywhere may change.
doModule :: forall m. (ModuVerse m) =>
            (S.ModuleName -> A.Decl SrcSpanInfo -> S.ModuleName)
         -> PathKey
         -> ModuleInfo
         -> m ()
doModule newModule _oldKey (ModuleInfo {module_ = A.Module _ _ _ _ decls, name_ = oldName}) =
    mapM_ doDecl decls
    where
      doDecl decl =
          case newModule oldName decl of
            newName | newName == oldName -> return ()
            newName -> moveDecl decl oldName newName
doModule _ _ _ = return ()

-- | Move decl from oldName to newName
moveDecl :: forall m. (ModuVerse m) =>
            A.Decl SrcSpanInfo
         -> S.ModuleName
         -> S.ModuleName
         -> m ()
moveDecl decl oldName newName = do
  -- moduleInfo %= Map.delete oldName
  modulesNew %= Map.adjust updateOldModule oldName
  modulesNew %= Map.adjust updateNewModule newName
  transferExports
  modulesNew %= Map.mapWithKey updateImports
  -- use moduleInfo >>= mapM_ (\name -> moduleInfo %= Map.adjustWithKey updateImports) . Map.keys
    where
      symbols = Set.fromList (symbolsDeclaredBy decl)

      -- Remove the declaration and any exports of its symbols from
      -- the old module.  Whether we need to add imports of these
      -- symbols must be specified by the AddImports flag.
      updateOldModule :: ModuleInfo -> ModuleInfo
      updateOldModule info@(ModuleInfo {module_ = A.Module l h p i decls}) =
          info {module_ = A.Module l (fmap removeOldExports h) p i (Prelude.filter (/= decl) decls)}
          where
            removeOldExports :: A.ModuleHead SrcSpanInfo -> A.ModuleHead SrcSpanInfo
            removeOldExports (A.ModuleHead l n w (Just (A.ExportSpecList l' exports))) =
                A.ModuleHead l n w (Just (A.ExportSpecList l' (Prelude.filter keep exports)))
                where
                  keep :: A.ExportSpec SrcSpanInfo -> Bool
                  keep e = foldDeclared (\name r -> r && Set.member name symbols) True e
            addImports :: A.Module SrcSpanInfo -> A.Module SrcSpanInfo
            addImports (A.Module l h p imports d) = A.Module l h p (imports ++ toImportSpec decl) d
      -- Add the declaration to the new module, Add the exports of the declaration's symbols to the new module
      updateNewModule :: ModuleInfo -> ModuleInfo
      updateNewModule (info@(ModuleInfo {module_ = A.Module l h p i d})) =
          info {module_ = A.Module l h p i (d ++ [decl])}
      updateNewModule info = info

      -- If the old module exported the symbols, move those exports to
      -- the new one.  It may not need them because the destination
      -- module might be the only module that used its symbols.
      -- Initially, we should have a flag that says whether to add
      -- these exports to the new module.  *** Eventually, we should
      -- check the moduverse for existing imports, and omit the
      -- exports if we don't find them.  Indeed, we could do this
      -- global analysis for all symbols, removing any exports of
      -- symbols that are never imported.
      transferExports :: m () -- Map S.ModuleName ModuleInfo -> Map S.ModuleName ModuleInfo
      transferExports = do
        (toMove, toKeep) <- partitionImports <$> use modulesNew
        modulesNew %= Map.adjust (\(info@(ModuleInfo {module_ = A.Module l h p i d})) ->
                                      info {module_ = A.Module l h p toKeep d}) oldName
        modulesNew %= Map.adjust (\(info@(ModuleInfo {module_ = A.Module l h p i d})) ->
                                      info {module_ = A.Module l h p (i ++ toMove) d}) newName
      -- Extract the exports of the symbols of decls
      partitionImports :: Map S.ModuleName ModuleInfo -> ([A.ImportDecl SrcSpanInfo], [A.ImportDecl SrcSpanInfo])
      partitionImports mp =
          case Map.lookup oldName mp of
            Just (ModuleInfo {module_ = A.Module l h p imports d}) ->
                let (a, b) = unzip (Prelude.map partitionImports' imports) in (concat a, concat b)
            Nothing -> ([], [])
            Just _ -> ([], [])
      partitionImports' :: A.ImportDecl SrcSpanInfo -> ([A.ImportDecl SrcSpanInfo], [A.ImportDecl SrcSpanInfo])
      partitionImports' (A.ImportDecl a m q src safe pkg as (Just (A.ImportSpecList l hiding@False specs))) =
          case partition (foldDeclared (\name r -> r && Set.member name symbols) True) specs of
            ([], _) -> ([], [A.ImportDecl a m q src safe pkg as (Just (A.ImportSpecList l False specs))])
            (xs, []) -> ([A.ImportDecl a m q src safe pkg as (Just (A.ImportSpecList l False xs))], [])
            (xs, ys) -> ([A.ImportDecl a m q src safe pkg as (Just (A.ImportSpecList l False xs))],
                         [A.ImportDecl a m q src safe pkg as (Just (A.ImportSpecList l False ys))])
      partitionImports' x = ([], [x])

      -- Remove the imports of the declaration's symbols from the new
      -- module and update all imports of the declaration's symbols in
      -- all (other) modules
      updateImports :: S.ModuleName -> ModuleInfo -> ModuleInfo
      updateImports moduleName info@(ModuleInfo {module_ = A.Module l h p imports d})
          | moduleName == oldName || moduleName == newName = info
      updateImports moduleName info@(ModuleInfo {module_ = A.Module l h p imports d}) =
          trace "implement me" info

writeChanges :: ModuVerse m => m ()
writeChanges = do
  mp <- use modulesNew
  mapM_ (\(name, info) -> liftIO (putStrLn (show name))) (Map.toList mp)

class ToImportSpec a where
    toImportSpec :: a -> [A.ImportDecl SrcSpanInfo]

instance ToImportSpec (A.Decl SrcSpanInfo) where
    toImportSpec (A.InstDecl {}) = []
    toImportSpec x = error ("unimplemented: toImportSpec " ++ show x)

{-
doModule :: (A.Decl SrcSpanInfo -> S.ModuleName) -> ModuleInfo -> m ()
doModule newModule info = do
  let oldModule = moduleName info
  
  -- Build a map describing how declarations are moving
  (newModuleMap :: Map (A.Decl SrcSpanInfo) (S.ModuleName, S.ModuleName))
      <- foldDecls (\d _ _ _ r -> if oldModule /= newModule d then Map.insert d (oldModule, newModule) r else r)
                   (\_ r -> r)
                   info
                   mempty
  case () of
    _ | member thisModule newModuleMap
-}
