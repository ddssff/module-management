{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Merge
    ( mergeModules
    ) where

import Control.Monad as List (mapM)
import Data.Foldable (fold)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.List as List (intercalate, map)
import Data.Map as Map (fromList, insert, lookup, Map, member, toAscList)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mempty)
import Data.Sequence ((|>))
import Data.Set as Set (difference, fromList, insert, Set, union)
import Data.Set.Extra as Set (mapM)
import Language.Haskell.Exts.Annotated.Simplify (sDecl, sExportSpec, sImportDecl, sModuleName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ExportSpecList(ExportSpecList), ImportDecl(..), Module(Module), ModuleHead(ModuleHead))
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(EModuleContents), ImportDecl(..), ModuleName(..))
import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldExports, foldHeader, foldImports, ignore, ignore2)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Internal (doResult, modifyParams, modulePath, ModuleResult(Modified, Removed, Unchanged), MonadClean(getParams), Params(moduVerse, testMode), parseModule, ModuleInfo)

-- | Merge the declarations from several modules into a single new
-- one, updating the imports of the modules in the moduVerse to
-- reflect the change.  It *is* permissable to use one of the input
-- modules as the output module.  Note that circular imports can be
-- created by this operation.
mergeModules :: MonadClean m => [S.ModuleName] -> S.ModuleName -> m (Set ModuleResult)
mergeModules inputs output =
    do Just univ <- getParams >>= return . moduVerse
       let univ' = union univ (Set.fromList (output : inputs))
       inputInfo <- loadModules inputs
       result <- Set.mapM (doModule inputInfo inputs output) univ' >>= Set.mapM doResult
      -- The inputs disappear and the output appears.  If the output is one
      -- of the inputs, it does not disappear.
       modifyParams (\ p -> p {moduVerse = fmap (\ s -> Set.insert output (Set.difference s (Set.fromList inputs))) (moduVerse p)})
       Set.mapM clean result
    where
      clean x =
          do flag <- getParams >>= return . not . testMode
             case x of
               (Modified name _) | flag -> modulePath name >>= cleanImports
               _ -> return x

-- Process one of the modules in the moduVerse and return the result.
-- The output module may not (yet) be an element of the moduVerse, in
-- that case choose the first input modules to convert into the output
-- module.
doModule :: MonadClean m => Map S.ModuleName (A.Module SrcSpanInfo, String, [Comment]) -> [S.ModuleName] -> S.ModuleName -> S.ModuleName -> m ModuleResult
doModule inputInfo inputs@(first : _) output name =
    do -- The new module will be based on the existing module, unless
       -- name equals output and output does not exist
       let oldName = if name == output && not (Map.member name inputInfo) then first else name
       (m, text, comments) <- maybe (loadModule oldName) return (Map.lookup name inputInfo)
       return $ case () of
         _ | name == output -> Modified name (doOutput inputInfo inputs output (m, text, comments))
           | elem name inputs -> Removed name
         _ -> let text' = doOther inputs output (m, text, comments) in
              if text /= text' then Modified name text' else Unchanged name
doModule _ [] _ _ = error "doModule: no inputs"

-- | Create the output module, the destination of the merge.
doOutput :: Map S.ModuleName ModuleInfo -> [S.ModuleName] -> S.ModuleName -> ModuleInfo -> String
doOutput inputInfo inNames outName m =
    header ++ exports ++ imports ++ decls
    where
      header = fold (foldHeader echo2 echo (\ _ pref _ suff r -> r |> pref <> prettyPrint outName <> suff) echo m mempty) <>
               fold (foldExports (\ s r -> r |> s <> maybe "" (intercalate ", " . List.map (prettyPrint)) (mergeExports inputInfo outName) <> "\n") ignore ignore2 m mempty)
      exports = fromMaybe "" (foldExports ignore2 (\ _e pref _ _ r -> maybe (Just pref) Just r) ignore2 m Nothing)
      imports = foldExports ignore2 ignore (\ s r -> r <> s {-where-}) m "" <>
                -- Insert the new imports just after the first "pre" string of the imports
                (fromMaybe "" (foldImports (\ _ pref _ _ r -> maybe (Just (pref <> unlines (List.map (moduleImports inputInfo) inNames))) Just r) m Nothing)) <>
                (foldImports (\ _i pref s suff r -> r <> pref <> s <> suff) m "")
      decls = fromMaybe "" (foldDecls (\ _d _ _ _ r -> Just (fromMaybe (unlines (List.map (moduleDecls inputInfo outName) inNames)) r)) (\ s r -> Just (maybe s (<> s) r)) m Nothing)

-- | Update a module that does not participate in the merge - this
-- involves changing imports and exports of merged modules.
-- (Shouldn't this also fix qualified symbols?)
doOther :: [S.ModuleName] -> S.ModuleName -> ModuleInfo -> String
doOther inputs output m =
    fold (foldHeader echo2 echo echo echo m mempty) <>
    fold (foldExports echo2 (\ x pref s suff r -> r |> pref <> fromMaybe s (fixModuleExport inputs output (sExportSpec x)) <> suff) echo2 m mempty) <>
    fold (foldImports (\ x pref s suff r -> r |> pref <> fromMaybe s (fixModuleImport inputs output (sImportDecl x)) <> suff) m mempty) <>
    fold (foldDecls echo echo2 m mempty)

fixModuleExport :: [S.ModuleName] -> S.ModuleName -> S.ExportSpec -> Maybe String
fixModuleExport inputs output x =
          case x of
            S.EModuleContents y
                | elem y inputs -> Just (prettyPrint (S.EModuleContents output))
            _ -> Nothing

fixModuleImport :: [S.ModuleName] -> S.ModuleName -> S.ImportDecl -> Maybe String
fixModuleImport inputs output x =
          case x of
            S.ImportDecl {S.importModule = y}
                | elem y inputs -> Just (prettyPrint (x {S.importModule = output}))
            _ -> Nothing

mergeExports :: Map S.ModuleName (A.Module SrcSpanInfo, String, [Comment]) -> S.ModuleName -> Maybe [S.ExportSpec]
mergeExports old new =
    Just (concatMap mergeExports' (Map.toAscList old))
    where
      mergeExports' (_, (A.Module _ Nothing _ _ _, _, _)) = error "mergeModules: no explicit export list"
      mergeExports' (_, (A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _, _, _)) = error "mergeModules: no explicit export list"
      mergeExports' (_, (A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList _ e)))) _ _ _, _, _)) = updateModuleContentsExports old new (List.map sExportSpec e)
      mergeExports' (_, _) = error "mergeExports'"

updateModuleContentsExports :: Map S.ModuleName (A.Module SrcSpanInfo, String, [Comment]) -> S.ModuleName -> [S.ExportSpec] -> [S.ExportSpec]
updateModuleContentsExports old new es =
    foldl f [] es
    where
      f :: [S.ExportSpec] -> S.ExportSpec ->  [S.ExportSpec]
      f ys (S.EModuleContents m) =
          let e' = S.EModuleContents (if Map.member m old then new else m) in
          ys ++ if elem e' ys then [] else [e']
      f ys e = ys ++ [e]

moduleImports :: Map S.ModuleName ModuleInfo -> S.ModuleName -> String
moduleImports old name =
    let (Just m) = Map.lookup name old in
    foldImports (\ x pref s suff r ->
                    r
                    -- If this is the first import, omit the prefix, it includes the ") where" text.
                    <> (if r == "" then "" else pref)
                    <> if Map.member (sModuleName (A.importModule x)) old then "" else (s <> suff))
                m "" <> "\n"

-- | Grab the declarations out of the old modules, fix any
-- qualified symbol references, prettyprint and return.
--
-- Bug: If we cat two modules A and B, and A imported a symbol from B
-- and referenced that symbol with a qualifier from an "as" import, the
-- as qualifier needs to be changed to a full qualifier.
--
-- In terms of what is going on right here, if m imports any of the
-- modules in oldmap with an "as" qualifier, identifiers using the
-- module name in the "as" qualifier must use new instead.
moduleDecls :: Map S.ModuleName ModuleInfo -> S.ModuleName -> S.ModuleName -> String
moduleDecls oldmap new name =
    let (Just m@(A.Module _ _ _ imports _, _, _)) = Map.lookup name oldmap in
    let oldmap' = foldr f oldmap imports in
    fold (foldDecls (\ d pref s suff r ->
                         let d' = sDecl d
                             d'' = fixReferences oldmap' new d' in
                         r |> pref <> (if d'' /= d' then prettyPrint d'' else s) <> suff)
                    echo2 m mempty)
    where
      f (A.ImportDecl _ m _ _ _ (Just a) _specs) mp =
          case Map.lookup (sModuleName m) oldmap of
            Just x -> Map.insert (sModuleName a) x mp
            _ -> mp
      f _ mp = mp

-- | Change any ModuleName in 'old' to 'new'.  Note that this will
-- probably mess up the location information, so the result (if
-- different from the original) should be prettyprinted, not
-- exactPrinted.
fixReferences :: (Data a, Typeable a) => Map S.ModuleName ModuleInfo -> S.ModuleName -> a -> a
fixReferences oldmap new x =
    everywhere (mkT moveModuleName) x
    where
      moveModuleName :: S.ModuleName -> S.ModuleName
      moveModuleName name@(S.ModuleName _) = if Map.member name oldmap then new else name

-- junk :: String -> Bool
-- junk s = isSuffixOf ".imports" s || isSuffixOf "~" s

loadModules :: MonadClean m => [S.ModuleName] -> m (Map S.ModuleName ModuleInfo)
loadModules names = List.mapM loadModule names >>= return . Map.fromList . zip names

loadModule :: MonadClean m => S.ModuleName -> m ModuleInfo
loadModule name = modulePath name >>= parseModule
