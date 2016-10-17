{-# LANGUAGE CPP, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Merge
    ( mergeModules
    ) where

import Control.Lens (use)
import Control.Monad as List (mapM, mapM_, when)
import Control.Exception.Lifted as IO (catch)
import Data.Foldable (fold)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.List as List (find, intercalate, map)
import Data.Map (keys)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Monoid ((<>))
import Data.Sequence as Seq ((<|), null, Seq, (|>))
import Data.Set as Set (fromList, toList, union)
#if MIN_VERSION_haskell_src_exts(1,18,0)
import qualified Language.Haskell.Exts.Syntax as A (ImportDecl(ImportDecl), Module(Module), ModuleHead(ModuleHead))
#else
import Language.Haskell.Exts.Annotated.Simplify (sDecl, sImportDecl, sModuleName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ImportDecl(ImportDecl), Module(Module), ModuleHead(ModuleHead))
#endif
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcInfo)
import qualified Language.Haskell.Exts.Syntax as S (ImportDecl(ImportDecl, importModule), ModuleName(..))
import Language.Haskell.Modules.Common (doResult, fixExport, ModuleResult(..), reportResult)
import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldExports, foldHeader, foldImports, ignore, ignore2, ModuleInfo(..))
import Language.Haskell.Modules.Imports (cleanResults)
import Language.Haskell.Modules.ModuVerse ({-moduleKey, modulesNew, modulesOrig,-} loadModule, loadModuleMaybe, moduVerse, ModuVerse, parseModule, parseModuleMaybe)
import Language.Haskell.Modules.SourceDirs (ModKey(_modName), modKey, modKeyMaybe)
import Language.Haskell.Modules.Util.QIO (qLnPutStr, quietly)
import Text.PrettyPrint.HughesPJClass (prettyShow)

-- | Merge the declarations from several modules into a single new
-- one, updating the imports of the modules in the moduVerse to
-- reflect the change.  It *is* permissable to use one of the input
-- modules as the output module.  Note that circular imports can be
-- created by this operation.
mergeModules :: ModuVerse m => [ModKey] -> ModKey -> m [ModuleResult]
mergeModules inNames outName =
    do qLnPutStr ("mergeModules: [" ++ intercalate ", " (map prettyShow inNames) ++ "] -> " ++ prettyShow outName)
       quietly $
         do univ <- (Set.fromList . keys) <$> use moduVerse
            let allNames = toList $ union univ (Set.fromList (outName : inNames))
            results <- List.mapM (doModule inNames outName) allNames
            results' <- List.mapM doResult results
            List.mapM_ (\ x -> qLnPutStr ("mergeModules: " ++ reportResult x)) results'
            cleanResults results'

-- Process one of the modules in the moduVerse and return the result.
-- The output module may not (yet) be an element of the moduVerse, in
-- that case choose the first input modules to convert into the output
-- module.
doModule :: ModuVerse m =>
            [ModKey]  -- The names of the merge input modules
         -> ModKey    -- The name of the merge destination module
         -> ModKey    -- The module we will work on
         -> m ModuleResult
doModule inNames@(_ : _) outName thisName =
    do -- The new module will be based on the first input module,
       -- though its name will be changed to outModule.
       inInfo@(firstInfo : _) <-
           List.mapM (\ key -> modKey key >>= loadModule) inNames
             `IO.catch` (\ (_ :: IOError) -> error $ "mergeModules - failure reading input modules: " ++ show inNames)
       outInfo <- modKeyMaybe ({-modulePathBase "hs"-} outName) >>= maybe (pure Nothing) loadModuleMaybe
       thisInfo <- modKeyMaybe ({-modulePathBase "hs"-} thisName) >>= maybe (pure Nothing) loadModuleMaybe
       let baseInfo@(ModuleInfo {module_ = A.Module _ _ _ _ _}) = fromMaybe firstInfo thisInfo
       when (isJust outInfo && notElem outName inNames) (error "mergeModules - if output module exist it must also be one of the input modules")
       case (thisName /= outName, List.find (\ x -> key_ x == thisName) inInfo) of 
         (True, Just info) ->
             return (ToBeRemoved (key_ info))
         _ ->
           let header =
                   fold (foldHeader echo2 echo (if thisName == outName
                                                then \ _ pref _ suff r -> r |> pref <> prettyShow outName <> suff
                                                else echo)
                                               echo baseInfo mempty)
               exports =
                   case baseInfo of
                     -- Is there an export list?
                     ModuleInfo {module_ = A.Module _ (Just (A.ModuleHead _ _ _ (Just _))) _ _ _} ->
                         let lparen = fold (foldExports (<|) ignore ignore2 baseInfo mempty)
                             newExports =
                                 if thisName == outName
                                 then -- This should be a reasonable string
                                      -- to join two export lists.
                                      let sep = map (\ c -> if c == '(' then ',' else c) lparen in
                                      -- The output module gets modified
                                      -- copies of all the input module
                                      -- export lists.
                                      intercalate sep $ filter (/= "") $ List.map (\ (_, info) -> fold (foldExports ignore2 (fixExport inNames outName thisName) ignore2 info mempty)) (zip inNames inInfo)
                                 else fold (foldExports ignore2 (fixExport inNames outName thisName) ignore2 baseInfo mempty)
                             rparen = fold (foldExports ignore2 ignore (<|) baseInfo mempty) in
                         lparen <> newExports <> rparen
                     ModuleInfo {module_ = A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _} -> "where\n\n"
                     _ -> ""
               imports =
                   if thisName == outName
                   then let pre = fold (foldImports (\ _ pref _ _ r -> if Seq.null r then r |> pref else r) baseInfo mempty)
                            newImports = unlines (List.map (\ info -> fold (foldImports (moduleImports inNames outName thisName) info mempty)) inInfo) in
                        pre <> newImports
                   else fold (foldImports (moduleImports inNames outName thisName) baseInfo mempty)
               decls =
                   if thisName == outName
                   then fromMaybe "" (foldDecls (\ _ _ _ _ r -> Just (fromMaybe (unlines (List.map (moduleDecls inNames outName thisName) inInfo)) r)) (\ s r -> Just (maybe s (<> s) r)) baseInfo Nothing)
                   else moduleDecls inNames outName thisName baseInfo
               text' = header <> exports <> imports <> decls in
           return $ case thisInfo of
                      Just (ModuleInfo {modtext_ = text, key_ = key}) ->
                          if text' /= text then ToBeModified key text' else Unchanged key
                      Nothing ->
                          ToBeCreated thisName text'
           -- return $ if text' /= text then Modified thisName (key_ thisInfo) text' else Unchanged thisName (key_ thisInfo)
doModule [] _ _ = error "doModule: no inputs"

moduleImports :: SrcInfo loc =>
                 [ModKey] -> ModKey -> ModKey
              -> A.ImportDecl loc -> String -> String -> String -> Seq String -> Seq String
moduleImports inKeys outKey thisKey x pref s suff r =
    case fmap (const ()) x of
      (S.ImportDecl {S.importModule = name})
          | notElem name (map _modName inKeys) -> r |> pref <> s <> suff
          | thisKey == outKey -> r
      x' -> r |> pref <> prettyPrint (x' {S.importModule = _modName outKey}) <> suff

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
moduleDecls :: [ModKey] -> ModKey -> ModKey -> ModuleInfo -> String
moduleDecls inKeys outKey thisKey info@(ModuleInfo (A.Module _ _ _ imports _) _ _ _ _) =
    -- Get the import list for this module
    let inNames = map _modName inKeys ++ if thisKey == outKey then mapMaybe qualifiedImportName imports else [] in
    fold (foldDecls (\ d pref s suff r ->
                         let d' = fmap (const ()) d
                             d'' = fixReferences inNames (_modName outKey) d' in
                         r |> pref <> (if d'' /= d' then prettyPrint d'' else s) <> suff)
                    echo2 info mempty)
    where
      -- Looking at an import, augment the map with the "as" name of a
      -- qualified import.  module and that module's info.
      qualifiedImportName :: A.ImportDecl l -> Maybe (S.ModuleName ())
      qualifiedImportName (A.ImportDecl _ m _ _ _ _ (Just a) _specs) =
          case elem (fmap (const ()) m) (map _modName inKeys) of
            True -> Just (fmap (const ()) a)
            _ -> Nothing
      qualifiedImportName _ = Nothing
moduleDecls _ _ _ (ModuleInfo m _ _ _ _) = error $ "Unsupported module type: " ++ show m

-- | Change any ModuleName in 'old' to 'new'.
fixReferences :: (Data a, Typeable a) => [S.ModuleName ()] -> S.ModuleName () -> a -> a
fixReferences oldNames new x =
    everywhere (mkT moveModuleName) x
    where
      moveModuleName :: S.ModuleName () -> S.ModuleName ()
      moveModuleName name@(S.ModuleName () _) = if elem name oldNames then new else name
