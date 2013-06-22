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
import Data.List as List (filter, intercalate, map, nub)
import Data.Map as Map (delete, elems, empty, filter, insertWith, Map, mapWithKey, lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (intersection, map, null, Set, union, toList, delete, fold, insert, empty, member, singleton, difference, filter, unions)
import Data.Set.Extra as Set (gFind, mapM)
import Language.Haskell.Exts (ParseResult(ParseOk, ParseFailed), fromParseResult)
import qualified Language.Haskell.Exts.Annotated as A (Decl, Module(Module), ModuleHead(ModuleHead), Name, ImportDecl(..), ImportSpecList(..))
import Language.Haskell.Exts.Extension (Extension(NoImplicitPrelude))
import Language.Haskell.Exts.Annotated.Simplify (sModuleName, sName, sImportDecl, sImportSpec)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrintWithMode, prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..), Name(..), ImportDecl(..))
import Language.Haskell.Modules.Fold (foldModule, foldHeader, foldExports, foldImports, foldDecls, echo, echo2, ignore, ignore2)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Params (modifyParams, modulePath, MonadClean, getParams, Params(sourceDirs, moduVerse, testMode, extensions), parseFile, runCleanT, ModuleResult(..), doResult)
import Language.Haskell.Modules.Util.QIO (noisily)
import Language.Haskell.Modules.Util.Symbols (symbols, imports, exports)
import Language.Haskell.Modules.Util.Test (diff, repoModules)
import Prelude hiding (writeFile)
import System.Cmd (system)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((<.>))
import Test.HUnit (assertEqual, Test(TestCase, TestList))

setAny :: (a -> Bool) -> Set a -> Bool
setAny f s = not (Set.null (Set.filter f s))

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
splitModule old =
    do univ <- getParams >>= return . fromMaybe (error "moduVerse not set") . moduVerse
       path <- modulePath old
       text <- liftIO $ readFile path
       parsed <- parseFile path >>= return . fromParseResult
       newFiles <- doSplit text univ parsed >>= return . collisionCheck univ
       -- Write the new modules
       setMapM_ doResult newFiles
       -- Clean the new modules
       setMapM_ doClean newFiles
       -- We have created some new modules, add them to the moduVerse
       modifyParams (\ p -> p {moduVerse = Just (Set.delete old (union (created newFiles) univ))})
    where
      collisionCheck univ s =
          if not (Set.null illegal)
          then error ("One or more module to be created by splitModule already exists: " ++ show (Set.toList illegal))
          else s
          where
            illegal = Set.intersection univ (created s)
      created :: Set ModuleResult -> Set S.ModuleName
      created = setMapMaybe (\ x -> case x of Created m _ -> Just m; _ -> Nothing)
      -- Make sure this isn't trying to clobber a module that exists (other than 'old'.)
      doClean :: MonadClean m => ModuleResult -> m ()
      doClean (Created m _) = doClean' m
      doClean (Modified m _) = doClean' m
      doClean (Removed _) = return ()
      doClean (Unchanged _) = return ()
      doClean' m =
          do flag <- getParams >>= return . not . testMode
             when flag (modulePath m >>= cleanImports >> return ())

justs :: Ord a => Set (Maybe a) -> Set a
justs = Set.fold (\ mx s -> maybe s (`Set.insert` s) mx) Set.empty

-- | Create the set of module results implied by the split -
-- creations, removals, and modifications.  This includes the changes
-- to the imports in modules that imported the original module.
doSplit :: MonadClean m => String -> Set S.ModuleName -> A.Module SrcSpanInfo -> m (Set ModuleResult)
doSplit _ _ (A.Module _ _ _ _ []) = return Set.empty -- No declarations - nothing to split
doSplit _ _ (A.Module _ _ _ _ [_]) = return Set.empty -- One declaration - nothing to split (but maybe we should anyway?)
doSplit _ _ (A.Module _ Nothing _ _ _) = throw $ userError $ "splitModule: no explicit header"
doSplit text univ m@(A.Module _ (Just (A.ModuleHead _ moduleName _ (Just _))) _ _ _) =
    Set.mapM (updateImports (sModuleName moduleName) symbolToModule) univ' >>= return . union splitModules
    where
      -- The name of the module to be split
      old = sModuleName moduleName
      -- Build a map from module name to the list of declarations that
      -- will be in that module.  All of these declarations used to be
      -- in moduleName.

      moduleDeclMap :: Map S.ModuleName [(A.Decl SrcSpanInfo, String)]
      moduleDeclMap = foldDecls (\ d pref s suff r -> Set.fold (\ sym mp -> insertWith (++) (subModuleName reExported internal old sym) [(d, pref <> s <> suff)] mp) r (symbols d)) ignore2 m text Map.empty

      -- This returns a set of maybe because there may be instance
      -- declarations, in which case we want an Instances module to
      -- appear in newModuleNames below.
      declared :: Set (Maybe S.Name)
      declared = foldDecls (\ d _pref _s _suff r -> Set.union (symbols d) r) ignore2 m text Set.empty

      exported :: Set S.Name
      exported = foldExports ignore2 (\ e _pref _s _suff r -> Set.union (justs (symbols e)) r) ignore2 m text Set.empty

      reExported :: Set S.Name
      reExported = difference exported (justs declared)

      internal :: Set S.Name
      internal = difference (justs declared) exported

      univ' = Set.delete old univ

      newModuleNames :: Set S.ModuleName
      newModuleNames = Set.map (subModuleName reExported internal old) (union declared (Set.map Just exported))

      -- The modules created from 'old'
      splitModules :: Set ModuleResult
      splitModules =
          union (Set.map newModule newModuleNames)
                (if member old newModuleNames then Set.empty else singleton (Removed old))

      -- Map from symbol name to the module that symbol will move to
      symbolToModule :: Map (Maybe S.Name) S.ModuleName
      symbolToModule =
          mp'
          where
            mp' = foldExports ignore2 (\ e _ _ _ r -> Set.fold f r (symbols e)) ignore2 m text mp
            mp = foldDecls (\ d _ _ _ r -> Set.fold f r (symbols d)) ignore2 m text Map.empty
            f sym mp'' =
                Map.insertWith
                   (\ a b -> if a /= b then error ("symbolToModule - two modules for " ++ show sym ++ ": " ++ show (a, b)) else a)
                   sym
                   (subModuleName reExported internal old sym)
                   mp''

      -- Build a new module given its name and the list of
      -- declarations it should contain.
      newModule :: S.ModuleName -> ModuleResult
      newModule name'@(S.ModuleName modName) =
          (if member name' univ then Modified else Created) name' $
          case Map.lookup name' moduleDeclMap of
            Nothing ->
                -- Build a module that re-exports a symbol
                foldHeader echo2 echo (\ _n pref _ suff r -> r <> pref <> modName <> suff) echo m text "" <>
                foldExports echo2 ignore ignore2 m text "" <>
                doSeps (foldExports ignore2 (\ e pref s suff r -> r <> if setAny (`member` reExported) (justs (symbols e)) then [(pref, s <> suff)] else []) (\ s r -> r ++ [("", s)]) m text []) <>
                foldImports (\ _i pref s suff r -> r <> pref <> s <> suff) m text ""
            Just modDecls ->
                -- Change the module name in the header
                foldHeader echo2 echo (\ _n pref _ suff r -> r <> pref <> modName <> suff) echo m text "" <>
                "    ( " {-foldExports echo2 ignore ignore2 m text ""-} <>
                intercalate "\n    , " (nub (List.map (prettyPrintWithMode defaultMode) (newExports modDecls))) <>
                "\n    ) where" {-foldExports ignore2 ignore echo2 m text ""-} <>
                -- The prefix of the imports section
                fromMaybe "" (foldImports (\ _i pref _ _ r -> maybe (Just pref) Just r) m text Nothing) <>
                unlines (List.map (prettyPrintWithMode defaultMode) (elems (newImports modDecls))) <> "\n" <>
                -- Grab the old imports
                fromMaybe "" (foldImports (\ _i pref s suff r -> Just (maybe (s <> suff) (\ l -> l <> pref <> s <> suff) r)) m text Nothing) <>
                -- fromMaybe "" (foldDecls (\ _d pref _ _ r -> maybe (Just pref) Just r) ignore2 m text Nothing) <>
                concatMap snd (reverse modDecls) <> "\n"
              where
                -- Build export specs of the symbols created by each declaration.
                newExports modDecls = nub (concatMap (exports . fst) modDecls)
                -- newImports :: Map ModuleName ImportDecl
                newImports modDecls =
                    mapWithKey toImportDecl (Map.delete name'
                                             (Map.filter (\ pairs ->
                                                              let declared = justs (Set.unions (List.map (symbols . fst) pairs)) in
                                                              not (Set.null (Set.intersection declared (referenced modDecls)))) moduleDeclMap))
                -- In this module, we need to import any module that declares a symbol
                -- referenced here.
                referenced modDecls = Set.map sName (gFind modDecls :: Set (A.Name SrcSpanInfo))
doSplit _ _ _ = error "splitModule'"

-- Re-construct a separated list
doSeps :: [(String, String)] -> String
doSeps [] = ""
doSeps ((_, hd) : tl) = hd <> concatMap (\ (a, b) -> a <> b) tl

-- | Update the imports to reflect the changed module names in symbolToModule.
updateImports :: MonadClean m => S.ModuleName -> Map (Maybe S.Name) S.ModuleName -> S.ModuleName -> m ModuleResult
updateImports old symbolToModule name =
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
    where
      updateImportDecl :: String -> A.ImportDecl SrcSpanInfo -> String
      updateImportDecl s i =
          if sModuleName (A.importModule i) == old
          then intercalate "\n" (List.map prettyPrint (updateImportSpecs i (A.importSpecs i)))
          else s

      updateImportSpecs :: A.ImportDecl SrcSpanInfo -> Maybe (A.ImportSpecList SrcSpanInfo) -> [S.ImportDecl]
      -- No spec list, import all the split modules
      updateImportSpecs i Nothing = List.map (\ x -> (sImportDecl i) {S.importModule = x}) (Map.elems symbolToModule)
      -- If flag is True this is a "hiding" import
      updateImportSpecs i (Just (A.ImportSpecList _ flag specs)) =
          concatMap (\ spec -> let xs = mapMaybe (\ sym -> Map.lookup sym symbolToModule) (toList (symbols spec)) in
                               List.map (\ x -> (sImportDecl i) {S.importModule = x, S.importSpecs = Just (flag, [sImportSpec spec])}) xs) specs

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
        moduleToDecls :: Map S.ModuleName [(A.Decl SrcSpanInfo, String)]
        moduleToDecls = foldDecls (\ d pref s suff r -> foldr (\ sym mp -> insertWith (++) (subModuleName old sym) [(d, pref <> s <> suff)] mp) r (symbols d))
                            ignore2 m text Map.empty

        -- Build the new modules
        newModules :: Map S.ModuleName String
        newModules = mapWithKey newModule moduleToDecls
{-      newReExporter =
            foldHeader echo2 echo echo echo  m text "" <>
            foldExports echo2 echo echo2 m text ""
            fromMaybe "" (foldImports (\ _i pref s suff r -> maybe (Just (pref <> s <> suff)) (Just . (<> (pref <> s <> suff))) r) m text Nothing) <>
            "\n" <>
            unlines (List.map (prettyPrintWithMode defaultMode) (elems (mapWithKey toImportDecl moduleToDecls))) -}
splitModule' _ _ _ = error "splitModule'"
-}

-- | What module should this symbol be moved to?
subModuleName :: Set S.Name -> Set S.Name -> S.ModuleName -> Maybe S.Name -> S.ModuleName
subModuleName reExported internal (S.ModuleName moduleName) name =
    S.ModuleName (moduleName <.> f (case name of
                                      Nothing -> "Instances"
                                      Just (S.Symbol s) -> s
                                      Just (S.Ident s) -> s))
    where
      f x =
          case name of
            Nothing -> "Instances"
            Just name' | member name' reExported -> "ReExported"
            Just name' ->
                (if member name' internal then "Internal." else "") <>
                case x of
                  -- Any symbol that starts with a letter is converted to a module name
                  -- by capitalizing and keeping the remaining alphaNum characters.
                  (c : s) | isAlpha c -> toUpper c : List.filter isAlphaNum s
                  _ -> "OtherSymbols"

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
tests = TestList [test1, test2, test3]

test1 :: Test
test1 =
    TestCase $
      do _ <- system "rsync -aHxS --delete testdata/original/ testdata/copy"
         runCleanT $
           do modifyParams (\ p -> p {sourceDirs = ["testdata/copy"], moduVerse = Just repoModules})
              splitModule (S.ModuleName "Debian.Repo.Package")
         (code, out, err) <- diff "testdata/splitresult" "testdata/copy"
         assertEqual "splitModule" (ExitSuccess, "", "") (code, out, err)

test2 :: Test
test2 =
    TestCase $
    do _ <- system "rsync -aHxS --delete testdata/split2/ testdata/copy"
       runCleanT $ noisily $ noisily $
         do modifyParams (\ p -> p {testMode = True,
                                    sourceDirs = ["testdata/copy"],
                                    -- extensions = NoImplicitPrelude : extensions p,
                                    moduVerse = Just (singleton (S.ModuleName "Split"))})
            splitModule (S.ModuleName "Split")
       (code, out, err) <- diff "testdata/split2-result" "testdata/copy"
       assertEqual "split2" (ExitSuccess, "", "") (code, out, err)

test3 :: Test
test3 =
    TestCase $
    do _ <- system "rsync -aHxS --delete testdata/split2/ testdata/copy"
       runCleanT $ noisily $ noisily $
         do modifyParams (\ p -> p {testMode = False,
                                    sourceDirs = ["testdata/copy"],
                                    -- extensions = NoImplicitPrelude : extensions p,
                                    moduVerse = Just (singleton (S.ModuleName "Split"))})
            splitModule (S.ModuleName "Split")
       (code, out, err) <- diff "testdata/split2-clean-result" "testdata/copy"
       assertEqual "split2" (ExitSuccess, "", "") (code, out, err)
