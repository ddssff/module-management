{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Split
    ( splitModule
    , tests
    ) where

import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Default (Default(def))
import Data.List as List (filter, intercalate, isPrefixOf, map, nub)
import Data.Map as Map (delete, elems, empty, filter, insertWith, Map, mapWithKey, insert, lookup, fromListWith, toList, keys)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (fromList, intersection, map, null, Set, union, toList, delete, fold, insert, empty, singleton, member)
import Data.Set.Extra as Set (gFind, mapM)
import Language.Haskell.Exts (ParseResult(ParseOk, ParseFailed))
import qualified Language.Haskell.Exts.Annotated as A (Decl, Module(Module), ModuleHead(ModuleHead), Name, ImportDecl(..), ImportSpecList(..))
import Language.Haskell.Exts.Annotated.Simplify (sModuleName, sName, sImportDecl, sImportSpec)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrintWithMode, prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..), Name(..), ImportDecl(..))
import Language.Haskell.Modules.Cat (repoModules)
import Language.Haskell.Modules.Fold (foldModule, foldHeader, foldExports, foldImports, foldDecls, echo, echo2, ignore2)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Params (modifyParams, modulePath, MonadClean, getParams, Params(sourceDirs, moduVerse, testMode), parseFile, runCleanT, ModuleResult(..), doResult)
import Language.Haskell.Modules.Util.Symbols (symbols, imports, exports)
import Prelude hiding (writeFile)
import System.Cmd (system)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath ((<.>))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual, Test(TestCase, TestList))

setMapMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
setMapMaybe p s = Set.fold f Set.empty s
    where f x s' = maybe s' (\ y -> Set.insert y s') (p x)

setMapM_ :: (Monad m, Ord b) => (a -> m b) -> Set a -> m ()
setMapM_ f s = do _ <- Set.mapM f s
                  return ()

-- | Split each of a module's declarations into a new module.  The
-- resulting modules may need to import some of the other split
-- modules, but we don't know which or how to avoid circular imports,
-- so a commented out list of imports is added.
-- | If the original module was M, the the split operation creates a
-- subdirectory M containing a module for each declaration of the
-- original module, and replaces M.hs with a module that imports each
-- of the split declarations that were originally exported.
splitModule :: MonadClean m => S.ModuleName -> m ()
splitModule old@(S.ModuleName name) =
    do univ <- getParams >>= return . fromMaybe (error "moduVerse not set") . moduVerse
       path <- modulePath old
       text <- liftIO $ readFile path
       parsed <- parseFile path
       newFiles <- case parsed of
                     ParseFailed _ _ -> throw (userError $ "Parse of " ++ name ++ " failed")
                     ParseOk m -> doModules text univ m
       -- Make sure splitModules isn't trying to clobber a module that exists (except for the module to be split.)
       let new :: Set S.ModuleName
           new = setMapMaybe (\ x -> case x of Created m _ -> Just m; _ -> Nothing) newFiles
           illegal = Set.intersection univ new
       when (not . Set.null $ illegal)
         (error $ "One or more module to be created by splitModule already exists: " ++ show (Set.toList illegal))
       -- Write the new modules
       setMapM_ doResult newFiles
       -- Clean the new modules
       setMapM_ doClean newFiles
       -- We have created some new modules, add them to the moduVerse
       modifyParams (\ p -> p {moduVerse = Just (Set.delete old (union new univ))})
    where
      doClean :: MonadClean m => ModuleResult -> m ()
      doClean (Created m _) = doClean' m
      doClean (Modified m _) = doClean' m
      doClean (Removed _) = return ()
      doClean (Unchanged _) = return ()
      doClean' m =
          do flag <- getParams >>= return . not . testMode
             when flag (modulePath m >>= cleanImports >> return ())

doModules :: MonadClean m => String -> Set S.ModuleName -> A.Module SrcSpanInfo -> m (Set ModuleResult)
doModules _ _ (A.Module _ _ _ _ []) = return Set.empty -- No declarations - nothing to split
doModules _ _ (A.Module _ _ _ _ [_]) = return Set.empty -- One declaration - nothing to split (but maybe we should anyway?)
doModules _ _ (A.Module _ Nothing _ _ _) = throw $ userError $ "splitModule: no explicit header"
doModules text univ (m@(A.Module _ (Just (A.ModuleHead _ moduleName _ (Just _))) _ _ _)) =
    Set.mapM updateImports (Set.delete (sModuleName moduleName) univ) >>= return . union (fromList . elems $ splitModules)
    where
      splitModules :: Map S.ModuleName ModuleResult
      splitModules = Map.insert (sModuleName moduleName) updatedModule $ mapWithKey newModule declMap

      updatedModule = maybe (Removed (sModuleName moduleName)) (newModule (sModuleName moduleName)) (Map.lookup (sModuleName moduleName) declMap)

      -- Build a map from module name to the list of declarations that
      -- will be in that module.  All of these declarations used to be
      -- in moduleName.
      declMap :: Map S.ModuleName [(A.Decl SrcSpanInfo, String)]
      declMap = foldDecls (\ d pref s suff r -> foldr (\ sym mp -> insertWith (++) (subModuleName (sModuleName moduleName) sym) [(d, pref <> s <> suff)] mp) r (symbols d)) ignore2 m text Map.empty
      -- Map from symbol name to the module that symbol will move to, plus the declaration of that symbol.
      splitMap' :: Map S.Name (S.ModuleName, [(A.Decl SrcSpanInfo, String)])
      splitMap' =
          mp'
          where
            mp' = foldExports ignore2 (\ e _ _ _ r -> foldr (f []) r (symbols e)) ignore2 m text mp
            mp = foldDecls (\ d pref s suff r -> foldr (f [(d, pref <> s <> suff)]) r (symbols d)) ignore2 m text Map.empty
            f prs sym mp'' =
                Map.insertWith
                   (\ (a, l1) (b, l2) -> if a /= b then error ("splitMap - two modules for " ++ show sym ++ ": " ++ show (a, b)) else (a, l1 ++ l2))
                   sym
                   (subModuleName (sModuleName moduleName) sym, prs)
                   mp''
      -- The reverse of splitMap' - map from module name to the
      -- symbols it will contain.
      splitMap :: Map S.ModuleName (Set S.Name)
      splitMap = Map.fromListWith union . List.map (\ (a, (b, _)) -> (b, singleton a)) . Map.toList $ splitMap'

      newModule :: S.ModuleName -> [(A.Decl SrcSpanInfo, String)] -> ModuleResult
      newModule name'@(S.ModuleName modName) modDecls =
          (if member name' univ then Modified else Created) name' $
            -- Change the module name in the header
            foldHeader echo2 echo (\ _n pref _ suff r -> r <> pref <> modName <> suff) echo m text "" <>
            fromMaybe "" (foldExports ignore2 (\ _e pref _ _ r -> (maybe (Just pref) Just r)) ignore2 m text Nothing) <> "    ( " <>
            intercalate ", " (nub (List.map (prettyPrintWithMode defaultMode) newExports)) <>
            "\n    ) where" <>
            fromMaybe "" (foldImports (\ _i pref _ _ r -> maybe (Just pref) Just r) m text Nothing) <>
            unlines (List.map (prettyPrintWithMode defaultMode) (elems newImports)) <> "\n" <>
            -- Grab the old imports
            fromMaybe "" (foldImports (\ _i pref s suff r -> Just (maybe (s <> suff) (\ l -> l <> pref <> s <> suff) r)) m text Nothing) <>
            fromMaybe "" (foldDecls (\ _d pref _ _ r -> maybe (Just pref) Just r) ignore2 m text Nothing) <>
            concatMap snd (reverse modDecls) <> "\n"
          where
            -- Build export specs of the symbols created by each declaration.
            newExports = nub (concatMap (exports . fst) modDecls)
            -- newImports :: Map ModuleName ImportDecl
            newImports = mapWithKey toImportDecl (Map.delete name'
                                                         (Map.filter (\ pairs ->
                                                                          let declared = Set.fromList (concatMap (symbols . fst) pairs) in
                                                                          not (Set.null (Set.intersection declared referenced))) declMap))
            -- In this module, we need to import any module that declares a symbol
            -- referenced here.
            referenced :: Set S.Name
            referenced = Set.map sName (gFind modDecls :: Set (A.Name SrcSpanInfo))
      -- Update the imports to reflect the declMap here
      updateImports :: MonadClean m => S.ModuleName -> m ModuleResult
      updateImports name =
          do path <- modulePath name
             text' <- liftIO $ readFile path
             parsed <- parseFile path
             case parsed of
               ParseOk m' ->
                   let text'' = foldModule echo2 echo echo echo echo2 echo echo2
                                           (\ i pref s suff r -> r <> pref <> updateImportDecl s i <> suff)
                                           echo echo2 m' text' "" in
                   return $ if text' /= text'' then Modified name text'' else Unchanged name
               ParseFailed _ _ -> error $ "Parse error in " ++ show name
      updateImportDecl :: String -> A.ImportDecl SrcSpanInfo -> String
      updateImportDecl s i =
          if sModuleName (A.importModule i) == sModuleName moduleName
          then intercalate "\n" (List.map prettyPrint (updateImportSpecs i (A.importSpecs i)))
          else s
      updateImportSpecs :: A.ImportDecl SrcSpanInfo -> Maybe (A.ImportSpecList SrcSpanInfo) -> [S.ImportDecl]
      -- No spec list, import all the split modules
      updateImportSpecs i Nothing = List.map (\ x -> (sImportDecl i) {S.importModule = x}) (Map.keys splitMap)
      -- If flag is True this is a "hiding" import
      updateImportSpecs i (Just (A.ImportSpecList _ flag specs)) =
          concatMap (\ spec -> let xs = List.map fst (mapMaybe (\ sym -> Map.lookup sym splitMap') (symbols spec)) in
                               List.map (\ x -> (sImportDecl i) {S.importModule = x, S.importSpecs = Just (flag, [sImportSpec spec])}) xs) specs
{-
      updateImportSpecs :: S.ImportDecl -> Maybe (Bool, [S.ImportSpec]) -> [S.ImportDecl]
      updateImportSpecs Nothing = [S.ImportDecl ]
      updateImportSpecs Just (True, specs) = Just (True, List.map updateImportSpec specs)
      updateImportSpecs Just (False, specs = Just (False, List.map updateImportSpecs specs)
      updateImportSpec :: S.ImportSpec -> S.ImportSpec
      updateImportSpec (flag, S.IVar nm) = undefined
      updateImportSpec (flag, S.IAbs nm) = undefined
      updateImportSpec (flag, S.IThingAll nm) = undefined
      updateImportSpec (flag, S.IThingWith nm cs) = undefined
-}

doModules _ _ _ = error "splitModule'"

{-
splitModule' :: S.ModuleName -> ParseResult (A.Module SrcSpanInfo) -> String -> Map S.ModuleName String
splitModule' _ (ParseOk (A.Module _ _ _ _ [])) _ = empty -- No declarations - nothing to split
splitModule' _ (ParseOk (A.Module _ _ _ _ [_])) _ = empty -- One declaration - nothing to split
splitModule' (S.ModuleName s) (ParseFailed _ _) _ = throw $ userError $ "Parse of " ++ s ++ " failed"
splitModule' (S.ModuleName s) (ParseOk (A.Module _ Nothing _ _ _)) _ = throw $ userError $ "splitModule: " ++ s ++ " has no explicit header"
splitModule' (S.ModuleName s) (ParseOk (A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _)) _ = throw $ userError $ "splitModule: " ++ s ++ " has no explicit export list"
splitModule' _ (ParseOk (m@(A.Module _ (Just (A.ModuleHead _ moduleName _ (Just _))) _ _ _))) text =
    {- Map.insert name newReExporter $ -} newModules
    where
        -- Build a map from module name to the list of declarations that will be in that module.
        declMap :: Map S.ModuleName [(A.Decl SrcSpanInfo, String)]
        declMap = foldDecls (\ d pref s suff r -> foldr (\ sym mp -> insertWith (++) (subModuleName (sModuleName moduleName) sym) [(d, pref <> s <> suff)] mp) r (symbols d))
                            ignore2 m text Map.empty

        -- Build the new modules
        newModules :: Map S.ModuleName String
        newModules = mapWithKey newModule declMap
{-      newReExporter =
            foldHeader echo2 echo echo echo  m text "" <>
            foldExports echo2 echo echo2 m text ""
            fromMaybe "" (foldImports (\ _i pref s suff r -> maybe (Just (pref <> s <> suff)) (Just . (<> (pref <> s <> suff))) r) m text Nothing) <>
            "\n" <>
            unlines (List.map (prettyPrintWithMode defaultMode) (elems (mapWithKey toImportDecl declMap))) -}
splitModule' _ _ _ = error "splitModule'"
-}

-- | What module should this symbol be moved to?
subModuleName :: S.ModuleName -> S.Name -> S.ModuleName
subModuleName (S.ModuleName moduleName) name =
    S.ModuleName (case name of
                    S.Symbol s -> maybe moduleName (\ sub -> moduleName <.> sub) (f s)
                    S.Ident s -> maybe moduleName (\ sub -> moduleName <.> sub) (f s))
    where
      f x =
          case List.filter isAlphaNum x of
            [] -> Nothing
            (c : s) -> if isAlpha c then Just (toUpper c : s) else Nothing

-- | Build an import of the symbols created by a declaration.
toImportDecl :: S.ModuleName -> [(A.Decl SrcSpanInfo, String)] -> S.ImportDecl
toImportDecl (S.ModuleName modName) decls =
    S.ImportDecl {S.importLoc = def,
                  S.importModule = S.ModuleName modName,
                  S.importQualified = False,
                  S.importSrc = False,
                  S.importPkg = Nothing,
                  S.importAs = Nothing,
                  S.importSpecs = Just (False, nub (concatMap (imports . fst) decls))}

tests :: Test
tests = TestList [test1]

test1 :: Test
test1 =
    TestCase $
      do _ <- system "rsync -aHxS --delete testdata/original/ testdata/copy"
         runCleanT $
           do modifyParams (\ p -> p {sourceDirs = ["testdata/copy"], moduVerse = Just repoModules})
              splitModule (S.ModuleName "Debian.Repo.Package")
         (code, out, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/splitresult", "testdata/copy"] ""
         let out' = unlines (List.filter (not . isPrefixOf "Only") (lines out))
         assertEqual "splitModule" (ExitFailure 1, "", "") (code, out', err)
