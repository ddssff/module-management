-- | Move declarations between modules.
{-# LANGUAGE CPP, FlexibleInstances, OverloadedLists, RankNTypes, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Move
    ( moveDeclsBy
    ) where

import Debug.Trace
import Control.Lens -- (at, use, (%=))
import Control.Monad ({-foldM,-} when)
import Control.Monad.Trans (liftIO)
import Data.List as List (foldl', partition)
import Data.Map as Map (adjust, insert, keys, lookup, Map, mapWithKey, member, toList, union)
import Data.Set as Set ({-difference,-} fromList, member)
import qualified Language.Haskell.Exts.Annotated as A
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S -- (ExportSpec(..), ImportDecl(..), Module(..), ModuleName(..), Name(..))
import Language.Haskell.Modules.Fold (foldDecls, ModuleInfo(..))
import Language.Haskell.Modules.ModuVerse (buildSymbolMap, buildDestinationMap,
                                           modulesNew, modulesOrig, ModuVerse)
import Language.Haskell.Modules.SourceDirs (PathKey)
--import Language.Haskell.Modules.Split (newModule)
import Language.Haskell.Modules.Symbols (FoldDeclared, foldDeclared, symbolsDeclaredBy)
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
  buildDestinationMap
  -- Insert information found in modulesOrig into modulesNew
  Map.toList <$> use modulesOrig >>= mapM_ copyModule
  -- Get the list of existing module names
  _oldNames <- (Set.fromList . Map.keys) <$> use modulesNew
  -- Find the list of modules to be created
  (Map.toList <$> use modulesNew) >>=
      return . foldl' (\mp (name, info) ->
                           foldDecls (\d _ _ _ mp' ->
                                          let name' = newModule name d in
                                          if Map.member name' mp'
                                          then mp'
                                          else Map.insert name' (setModuleName name' info) mp')
                                      (\_ mp' -> mp')
                                      info mp)
                      mempty >>= \newNames ->
      modulesNew %= Map.union newNames
  mapM_ (uncurry $ doModule newModule) . Map.toList =<< use modulesOrig
  writeChanges
    where
      copyModule :: (PathKey, ModuleInfo) -> m ()
      copyModule (_key, info@(ModuleInfo _ _ _ _ name)) = do
        modulesNew %= Map.insert (trace ("copyModule " ++ show name) name) info

class SetModuleName a where
    setModuleName :: S.ModuleName -> a -> a

instance SetModuleName ModuleInfo where
    setModuleName name info = info {module_ = setModuleName name (module_ info), name_ = name}

instance SetModuleName (A.Module l) where
    setModuleName (S.ModuleName s') (A.Module l (Just (A.ModuleHead l' (A.ModuleName l'' _s) w e)) p i ds) =
        (A.Module l (Just (A.ModuleHead l' (A.ModuleName l'' s') w e)) p i ds)
    setModuleName _ m = m

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
      symPred :: FoldDeclared a => a -> Bool
      symPred = foldDeclared (\name r -> r && Set.member name symbols) True

      -- Remove the declaration and any exports of its symbols from
      -- the old module.  Whether we need to add imports of these
      -- symbols must be specified by the AddImports flag.
      updateOldModule :: ModuleInfo -> ModuleInfo
      updateOldModule info@(ModuleInfo {module_ = A.Module l h p i decls}) =
          info {module_ = A.Module l (fmap removeOldExports h) p i (Prelude.filter (/= decl) decls)}
          where
            removeOldExports :: A.ModuleHead SrcSpanInfo -> A.ModuleHead SrcSpanInfo
            removeOldExports (A.ModuleHead l' n w (Just (A.ExportSpecList l'' exports))) =
                A.ModuleHead l' n w (Just (A.ExportSpecList l'' (Prelude.filter keep exports)))
                where
                  keep :: A.ExportSpec SrcSpanInfo -> Bool
                  keep e = symPred e
            removeOldExports x = x
            _addImports :: A.Module SrcSpanInfo -> A.Module SrcSpanInfo
            _addImports (A.Module l' h' p' imports d) = A.Module l' h' p' (imports ++ toImportSpec decl) d
            _addImports x = x
      updateOldModule info = info
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
        (toMove, toKeep) <- partitionExports <$> use modulesNew
        modulesNew %= Map.adjust (putExports toKeep) oldName
        modulesNew %= Map.adjust (addExports toMove) newName

      -- Extract the exports of the symbols of decls
      partitionExports :: Map S.ModuleName ModuleInfo -> ([A.ExportSpec SrcSpanInfo], [A.ExportSpec SrcSpanInfo])
      partitionExports mp =
          case Map.lookup oldName mp of
            Just (ModuleInfo {module_ = A.Module _l (Just (A.ModuleHead _l' _n _w (Just (A.ExportSpecList _l'' exports)))) _p _i _d}) ->
                partition symPred exports
            Just (ModuleInfo {module_ = A.Module _l Nothing _p _i _d}) -> ([], [])
            Nothing -> ([], [])
            Just _ -> ([], [])

      putExports :: [A.ExportSpec SrcSpanInfo] -> ModuleInfo -> ModuleInfo
      putExports specs info@(ModuleInfo {module_ = A.Module l (Just (A.ModuleHead l' n w (Just (A.ExportSpecList l'' _)))) p i d}) =
          info {module_ = A.Module l (Just (A.ModuleHead l' n w (Just (A.ExportSpecList l'' specs)))) p i d}
      putExports _specs x = x

      addExports :: [A.ExportSpec SrcSpanInfo] -> ModuleInfo -> ModuleInfo
      addExports specs info@(ModuleInfo {module_ = A.Module l (Just (A.ModuleHead l' n w (Just (A.ExportSpecList l'' specs')))) p i d}) =
          info {module_ = A.Module l (Just (A.ModuleHead l' n w (Just (A.ExportSpecList l'' (specs' ++ specs))))) p i d}
      addExports _specs x = x

      -- Remove the imports of the declaration's symbols from the new
      -- module and update all imports of the declaration's symbols in
      -- all (other) modules.
      updateImports :: S.ModuleName -> ModuleInfo -> ModuleInfo
      updateImports moduleName info@(ModuleInfo {module_ = A.Module l h p imports d}) =
          info {module_ = A.Module l h p (updateImports' moduleName imports) d}
      updateImports _ info = info
      updateImports' :: S.ModuleName -> [A.ImportDecl SrcSpanInfo] -> [A.ImportDecl SrcSpanInfo]
      updateImports' moduleName imports
          | moduleName == oldName =
              -- Does the destination module import anything from this
              -- module?  If so this module must not import from
              -- there, lest it create a circular import.  If any uses
              -- of the departing symbols remain here it will create
              -- a compile error and cleanup will fail.  In the future
              -- we could examine all the declarations and move them
              -- with the departing declaration as a group.

              -- If the destination module doesn't import from this
              -- module we can add imports of the departing symbols.
              -- The subsequent cleanup will delete them if possible.
              trace ("implement me: updateImports1 " ++ show moduleName ++ " == " ++ show oldName) imports
      updateImports' moduleName imports
          | moduleName == newName =
              -- Remove imports of the arriving symbols.
              trace ("implement me: updateImports2 " ++ show moduleName ++ " == " ++ show newName) imports
      updateImports' _moduleName imports =
          -- Update the module name of symbols being moved.
          {-trace ("implement me: updateImports3 " ++ show moduleName ++ " /= " ++ show newName ++ ", " ++ show oldName)-} imports

      _partitionImports :: Map S.ModuleName ModuleInfo -> ([A.ImportDecl SrcSpanInfo], [A.ImportDecl SrcSpanInfo])
      _partitionImports mp =
          case Map.lookup oldName mp of
            Just (ModuleInfo {module_ = A.Module _l _h _p imports _d}) ->
                let (a, b) = unzip (Prelude.map partitionImports' imports) in (concat a, concat b)
            Nothing -> ([], [])
            Just _ -> ([], [])
      partitionImports' :: A.ImportDecl SrcSpanInfo -> ([A.ImportDecl SrcSpanInfo], [A.ImportDecl SrcSpanInfo])
      partitionImports' (A.ImportDecl a m q src safe pkg as (Just (A.ImportSpecList l _hiding@False specs))) =
          case partition symPred specs of
            ([], _) -> ([], [A.ImportDecl a m q src safe pkg as (Just (A.ImportSpecList l False specs))])
            (xs, []) -> ([A.ImportDecl a m q src safe pkg as (Just (A.ImportSpecList l False xs))], [])
            (xs, ys) -> ([A.ImportDecl a m q src safe pkg as (Just (A.ImportSpecList l False xs))],
                         [A.ImportDecl a m q src safe pkg as (Just (A.ImportSpecList l False ys))])
      partitionImports' x = ([], [x])


writeChanges :: ModuVerse m => m ()
writeChanges = do
  new <- use modulesNew
  mapM_ (\(_name, newInfo) -> do
           Just oldInfo <- Map.lookup (key_ newInfo) <$> use modulesOrig
           when (newInfo /= oldInfo) (liftIO $ putStrLn ("modified: " ++ show (key_ newInfo)))) (Map.toList new)
{-
  mp <- use modulesNew
  mapM_ (\(name, info) -> liftIO (putStrLn (show name))) (Map.toList mp)
-}

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
