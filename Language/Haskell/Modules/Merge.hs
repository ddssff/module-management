{-# LANGUAGE CPP, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Merge
    ( mergeModules
    ) where

import Control.Monad as List (mapM, when)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch)
import Data.Foldable (fold)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.List as List (find, intercalate, map)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Monoid ((<>), mempty)
import Data.Sequence as Seq ((<|), null, Seq, (|>))
import Data.Set as Set (fromList, toList, union)
import Language.Haskell.Exts.Annotated.Simplify (sDecl, sImportDecl, sModuleName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ImportDecl(ImportDecl), Module(Module), ModuleHead(ModuleHead))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcInfo)
import qualified Language.Haskell.Exts.Syntax as S (ImportDecl(ImportDecl, importModule), ModuleName(..))
import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldExports, foldHeader, foldImports, ignore, ignore2)
import Language.Haskell.Modules.Imports (cleanResults)
import Language.Haskell.Modules.ModuVerse (getNames, ModuleInfo(..), moduleName, parseModule, parseModuleMaybe)
import Language.Haskell.Modules.Params (doResult, fixExport, ModuleResult(..), MonadClean)
import Language.Haskell.Modules.SourceDirs (modulePathBase, pathKey, pathKeyMaybe)
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
            cleanResults results
    where
      reportResult x@(Modified _ key _) = qLnPutStr ("mergeModules: modifying " ++ show key) >> return x
      reportResult x@(Created name _) = qLnPutStr ("mergeModules: creating " ++ show name) >> return x
      reportResult x@(Removed _ key) = qLnPutStr ("mergeModules: removing " ++ show key) >> return x
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
doModule inNames@(_ : _) outName thisName =
    do -- The new module will be based on the first input module,
       -- though its name will be changed to outModule.
       inInfo@(firstInfo : _) <-
           List.mapM (\ name -> pathKey (modulePathBase "hs" name) >>= parseModule) inNames
             `IO.catch` (\ (_ :: IOError) -> error $ "mergeModules - failure reading input modules: " ++ show inNames)
       outInfo <- pathKeyMaybe (modulePathBase "hs" outName) >>= parseModuleMaybe
       thisInfo <- pathKeyMaybe (modulePathBase "hs" thisName) >>= parseModuleMaybe
       let baseInfo@(ModuleInfo {module_ = A.Module _ _ _ _ _}) = fromMaybe firstInfo thisInfo
       when (isJust outInfo && notElem outName inNames) (error "mergeModules - if output module exist it must also be one of the input modules")
       case (thisName /= outName, List.find (\ x -> moduleName x == thisName) inInfo) of 
         (True, Just info) ->
             return (Removed thisName (key_ info))
         _ ->
           let header =
                   fold (foldHeader echo2 echo (if thisName == outName
                                                then \ _ pref _ suff r -> r |> pref <> prettyPrint outName <> suff
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
                      Just (ModuleInfo {text_ = text, key_ = key}) ->
                          if text' /= text then Modified thisName key text' else Unchanged thisName key
                      Nothing ->
                          Created thisName text'
           -- return $ if text' /= text then Modified thisName (key_ thisInfo) text' else Unchanged thisName (key_ thisInfo)
doModule [] _ _ = error "doModule: no inputs"

moduleImports :: SrcInfo loc =>
                 [S.ModuleName] -> S.ModuleName -> S.ModuleName
              -> A.ImportDecl loc -> String -> String -> String -> Seq String -> Seq String
moduleImports inNames outName thisName x pref s suff r =
    case sImportDecl x of
      (S.ImportDecl {S.importModule = name})
          | notElem name inNames -> r |> pref <> s <> suff
          | thisName == outName -> r
      x' -> r |> pref <> prettyPrint (x' {S.importModule = outName}) <> suff

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
moduleDecls :: [S.ModuleName] -> S.ModuleName -> S.ModuleName -> ModuleInfo -> String
moduleDecls inNames outName thisName info@(ModuleInfo (A.Module _ _ _ imports _) _ _ _) =
    -- Get the import list for this module
    let inNames' = inNames ++ if thisName == outName then mapMaybe qualifiedImportName imports else [] in
    fold (foldDecls (\ d pref s suff r ->
                         let d' = sDecl d
                             d'' = fixReferences inNames' outName d' in
                         r |> pref <> (if d'' /= d' then prettyPrint d'' else s) <> suff)
                    echo2 info mempty)
    where
      -- Looking at an import, augment the map with the "as" name of a
      -- qualified import.  module and that module's info.
      qualifiedImportName :: A.ImportDecl l -> Maybe S.ModuleName
      qualifiedImportName (A.ImportDecl _ m _ _ _ (Just a) _specs) =
          case elem (sModuleName m) inNames of
            True -> Just (sModuleName a)
            _ -> Nothing
      qualifiedImportName _ = Nothing
moduleDecls _ _ _ (ModuleInfo m _ _ _) = error $ "Unsupported module type: " ++ show m

-- | Change any ModuleName in 'old' to 'new'.
fixReferences :: (Data a, Typeable a) => [S.ModuleName] -> S.ModuleName -> a -> a
fixReferences oldNames new x =
    everywhere (mkT moveModuleName) x
    where
      moveModuleName :: S.ModuleName -> S.ModuleName
      moveModuleName name@(S.ModuleName _) = if elem name oldNames then new else name
