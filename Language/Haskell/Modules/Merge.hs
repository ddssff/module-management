{-# LANGUAGE CPP, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Merge
    ( mergeModules
    ) where

import Debug.Trace

import Control.Monad as List (mapM, when)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch)
import Control.Monad.Trans (liftIO)
import Data.Foldable (fold)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.List as List (intercalate, map, nub)
import Data.Map as Map (insert, lookup, Map, member, fromList, keys)
import Data.Maybe (fromMaybe, isNothing, isJust, mapMaybe)
import Data.Monoid ((<>), mempty)
import Data.Sequence (Seq, (|>), (<|))
import Data.Set as Set (Set, fromList, toList, union)
import Data.Set.Extra as Set (mapM)
import Language.Haskell.Exts.Annotated.Simplify (sDecl, sExportSpec, sImportDecl, sModuleName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ExportSpec, ExportSpecList(ExportSpecList), ImportDecl(..), Module(Module), ModuleHead(ModuleHead))
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(EModuleContents), ImportDecl(..), ModuleName(..))
import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldExports, foldHeader, foldImports, ignore, ignore2)
import Language.Haskell.Modules.Imports (cleanImports, cleanResult)
import Language.Haskell.Modules.Internal (doResult, ModuleResult(Modified, Created, Removed, Unchanged), MonadClean(getParams), Params(testMode))
import Language.Haskell.Modules.ModuVerse (ModuVerse, ModuleInfo, getNames, getInfo, modulePath, parseModule, parseModule', moduleName)
import Language.Haskell.Modules.Util.QIO (qLnPutStr, quietly)

-- | Merge the declarations from several modules into a single new
-- one, updating the imports of the modules in the moduVerse to
-- reflect the change.  It *is* permissable to use one of the input
-- modules as the output module.  Note that circular imports can be
-- created by this operation.
mergeModules :: MonadClean m => [S.ModuleName] -> S.ModuleName -> m [ModuleResult]
mergeModules inNames outName =
    do qLnPutStr ("mergeModules: [" ++ intercalate ", " (map prettyPrint inNames) ++ "] -> " ++ prettyPrint outName)
       quietly $
         do univ <- getNames
            let allNames = toList $ union univ (Set.fromList (outName : inNames))
            results <- List.mapM (doModule inNames outName) allNames >>= List.mapM doResult >>= List.mapM reportResult
            List.mapM cleanResult results
    where
      reportResult x@(Modified (S.ModuleName name) _) = qLnPutStr ("mergeModules: modifying " ++ name) >> return x
      reportResult x@(Created (S.ModuleName name) _) = qLnPutStr ("mergeModules: creating " ++ name) >> return x
      reportResult x@(Removed (S.ModuleName name)) = qLnPutStr ("mergeModules: removing " ++ name) >> return x
      reportResult x = return x

-- Process one of the modules in the moduVerse and return the result.
-- The output module may not (yet) be an element of the moduVerse, in
-- that case choose the first input modules to convert into the output
-- module.
doModule :: MonadClean m =>
            [S.ModuleName]  -- The names of the merge input modules
         -> S.ModuleName    -- The name of the merge destination module
         -> S.ModuleName    -- The module we will work on
         -> m ModuleResult
doModule inNames@(baseName : _) outName thisName =
    do -- The new module will be based on the first input module,
       -- though its name will be changed to outModule.
       inInfo@(firstInfo@(m, text, _) : _) <-
           List.mapM (\ name -> modulePath name >>= parseModule) inNames
             `IO.catch` (\ (e :: IOError) -> error $ "mergeModules - failure reading input modules: " ++ show inNames)
       outInfo <- modulePath outName >>= parseModule'
       thisInfo <- modulePath thisName >>= parseModule'
       let baseInfo = fromMaybe firstInfo thisInfo
       when (isJust outInfo && notElem outName inNames) (error "mergeModules - if output module exist it must also be one of the input modules")
       case thisName /= outName && elem thisName inNames of
         True -> return (Removed thisName)
         False ->
           let header =
                   fold (foldHeader echo2 echo (if thisName == outName
                                                then \ _ pref _ suff r -> r |> pref <> prettyPrint outName <> suff
                                                else echo)
                                               echo baseInfo mempty)
               exports =
                   let lparen = fold (foldExports (<|) ignore ignore2 baseInfo mempty)
                       newExports =
                           if thisName == outName
                           then -- This should be a reasonable string
                                -- to join two export lists.
                                let sep = map (\ c -> if c == '(' then ',' else c) lparen in
                                -- The output module gets modified
                                -- copies of all the input module
                                -- export lists.
                                intercalate sep $ List.map (\ (mergeName, info) -> fold (foldExports ignore2 (fixExport inNames outName thisName) ignore2 info mempty)) (zip inNames inInfo)
                           else fold (foldExports ignore2 (fixExport inNames outName thisName) ignore2 baseInfo mempty)
                       rparen = fold (foldExports ignore2 ignore (<|) baseInfo mempty) in
                   lparen <> newExports <> rparen
               imports =
                   if thisName == outName
                   then let newImports = unlines (List.map (moduleImports inNames) inInfo) in
                        foldImports (\ _i pref s suff r -> r <> pref <> (if r == "" then newImports else "") <> s <> suff) baseInfo ""
                   else fold (foldImports (\ x pref s suff r -> r |> pref <> fromMaybe s (fixModuleImport inNames outName (sImportDecl x)) <> suff) baseInfo mempty)
               decls =
                   if thisName == outName
                   then fromMaybe "" (foldDecls (\ _ _ _ _ r -> Just (fromMaybe (unlines (List.map (moduleDecls inNames outName) inInfo)) r)) (\ s r -> Just (maybe s (<> s) r)) baseInfo Nothing)
                   else fold (foldDecls echo echo2 baseInfo mempty)
               text' = header <> exports <> imports <> decls in
           return $ if text' /= text then Modified thisName text' else Unchanged thisName
doModule [] _ _ = error "doModule: no inputs"

-- | Update an export spec from input module @name@ for inclusion in
-- the output module.  If it is a re-export of one of the input modules,
-- it should be omitted if we are building the output module, or changed
-- to the output module if we are building some other module.
fixExport :: [S.ModuleName] -> S.ModuleName -> S.ModuleName
          -> A.ExportSpec l -> String -> String -> String -> Seq String -> Seq String
fixExport inNames outName thisName e pref s suff r =
    case sExportSpec e of
      S.EModuleContents name
          -- when building the output module, omit re-exports of input modules
          | thisName == outName && elem name inNames -> r
          -- when building other modules, update re-exports of input modules
          | elem name inNames -> r |> pref <> prettyPrint (S.EModuleContents outName) <> suff
          -- Anything else is unchanged
      _ -> r |> pref <> s <> suff

fixModuleImport :: [S.ModuleName] -> S.ModuleName -> S.ImportDecl -> Maybe String
fixModuleImport inputs output x =
          case x of
            S.ImportDecl {S.importModule = y}
                | elem y inputs -> Just (prettyPrint (x {S.importModule = output}))
            _ -> Nothing

moduleImports :: [S.ModuleName] -> ModuleInfo -> String
moduleImports inNames info =
    foldImports (\ x pref s suff r ->
                    r
                    -- If this is the first import, omit the prefix, it includes the ") where" text.
                    <> (if r == "" then "" else pref)
                    <> if elem (sModuleName (A.importModule x)) inNames then "" else (s <> suff))
                info "" <> "\n"

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
moduleDecls :: [S.ModuleName] -> S.ModuleName -> ModuleInfo -> String
moduleDecls oldNames new info@(A.Module _ _ _ imports _, _, _) =
    -- Get the import list for this module
    let oldNames' = oldNames ++ mapMaybe qualifiedImportName imports in
    fold (foldDecls (\ d pref s suff r ->
                         let d' = sDecl d
                             d'' = fixReferences oldNames' new d' in
                         r |> pref <> (if d'' /= d' then prettyPrint d'' else s) <> suff)
                    echo2 info mempty)
    where
      -- Looking at an import, augment the map with the "as" name of a
      -- qualified import.  module and that module's info.
      qualifiedImportName :: A.ImportDecl l -> Maybe S.ModuleName
      qualifiedImportName (A.ImportDecl _ m _ _ _ (Just a) _specs) =
          case elem (sModuleName m) oldNames of
            True -> Just (sModuleName a)
            _ -> Nothing
      qualifiedImportName _ = Nothing

-- | Change any ModuleName in 'old' to 'new'.
fixReferences :: (Data a, Typeable a) => [S.ModuleName] -> S.ModuleName -> a -> a
fixReferences oldNames new x =
    everywhere (mkT moveModuleName) x
    where
      moveModuleName :: S.ModuleName -> S.ModuleName
      moveModuleName name@(S.ModuleName _) = if elem name oldNames then new else name

-- junk :: String -> Bool
-- junk s = isSuffixOf ".imports" s || isSuffixOf "~" s
