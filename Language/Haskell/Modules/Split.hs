{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Split
    ( splitModule
    , tests
    ) where

import Control.Exception (throw)
import Control.Monad.Trans (liftIO)
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Default (Default(def))
import Data.List as List (filter, intercalate, isPrefixOf, map, nub)
import Data.Map as Map (delete, elems, empty, filter, insert, insertWith, keys, Map, mapWithKey, toList)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (fromList, intersection, map, null, Set)
import Data.Set.Extra (gFind)
import Language.Haskell.Exts (ParseResult(ParseOk, ParseFailed))
import qualified Language.Haskell.Exts.Annotated as A (Decl, Module(Module), ModuleHead(ModuleHead), Name)
import Language.Haskell.Exts.Annotated.Simplify (sModuleName, sName)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..), Name(..), ExportSpec, ImportSpec, ImportDecl(..))
import Language.Haskell.Modules.Fold (foldModule)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Params (modifyParams, modulePath, MonadClean, Params(sourceDirs), parseFile, runCleanT)
import Language.Haskell.Modules.Util.DryIO (createDirectoryIfMissing, replaceFile, tildeBackup)
import Language.Haskell.Modules.Util.QIO (noisily)
import Language.Haskell.Modules.Util.Symbols (symbols, imports, exports)
import Prelude hiding (writeFile)
import System.Cmd (system)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath ((<.>), dropExtension)
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual, Test(TestCase, TestList))

subModuleName :: S.ModuleName -> S.Name -> S.ModuleName
subModuleName (S.ModuleName moduleName) name =
    S.ModuleName (case name of
                    S.Symbol s -> moduleName <.> f s
                    S.Ident s -> moduleName <.> f s)
    where
      f x =
          case List.filter isAlphaNum x of
            [] -> "OtherSymbols"
            (c : s) -> if isAlpha c then toUpper c : s else throw $ userError $ "symbolToModuleName: must begin with a letter: " ++ show x

-- | Split each of a module's declarations into a new module.  The
-- resulting modules may need to import some of the other split
-- modules, but we don't know which or how to avoid circular imports,
-- so a commented out list of imports is added.
splitModule :: MonadClean m => S.ModuleName -> m ()
splitModule name =
    do path <- modulePath name
       parsed <- parseFile path
       text <- liftIO $ readFile path
       let newFiles = splitModule' name parsed text
       -- Create a subdirectory named after the old module
       createDirectoryIfMissing True (dropExtension path)
       -- Write the new modules
       mapM_ (uncurry writeModule) (Map.toList newFiles)
       -- Clean the new modules
       mapM_ (\ name' -> modulePath name' >>= cleanImports) (Map.keys newFiles)

writeModule :: MonadClean m => S.ModuleName -> String -> m ()
writeModule name text = modulePath name >>= \ path -> replaceFile tildeBackup path text

-- | If the original module was M, the the split operation creates a
-- subdirectory M containing a module for each declaration of the
-- original module, and replaces M.hs with a module that imports each
-- of the split declarations that were originally exported.
splitModule' :: S.ModuleName -> ParseResult (A.Module SrcSpanInfo) -> String -> Map S.ModuleName String
splitModule' _ (ParseOk (A.Module _ _ _ _ [])) _ = empty -- No declarations - nothing to split
splitModule' _ (ParseOk (A.Module _ _ _ _ [_])) _ = empty -- One declaration - nothing to split
splitModule' (S.ModuleName s) (ParseFailed _ _) _ = throw $ userError $ "Parse of " ++ s ++ " failed"
splitModule' (S.ModuleName s) (ParseOk (A.Module _ Nothing _ _ _)) _ = throw $ userError $ "splitModule: " ++ s ++ " has no explicit header"
splitModule' (S.ModuleName s) (ParseOk (A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _)) _ = throw $ userError $ "splitModule: " ++ s ++ " has no explicit export list"
splitModule' name (ParseOk (m@(A.Module _ (Just (A.ModuleHead _ moduleName _ (Just _))) _ _ _))) text =
    Map.insert name newReExporter $ newModules
    where
        -- Build the new modules
        newModules :: Map S.ModuleName String
        newModules = mapWithKey newModule declMap

        newModule name'@(S.ModuleName modName) modDecls =
            header <>
            exportsPrefix <> "    ( " <>
            intercalate ", " (nub (List.map (prettyPrintWithMode defaultMode) newExports)) <>
            "\n    ) where" <>
            importsPrefix <> unlines (List.map (prettyPrintWithMode defaultMode) (elems newImports)) <> "\n" <> oldImports <>
            declPrefix <> concatMap snd (reverse modDecls) <> "\n"
            where
              newExports = nub (concatMap toExportSpecs modDecls)
              -- In this module, we need to import any module that declares a symbol
              -- referenced here.
              referenced :: Set S.Name
              referenced = Set.map sName (gFind modDecls :: Set (A.Name SrcSpanInfo))
              -- newImports :: Map ModuleName ImportDecl
              newImports = mapWithKey toImportDecl (Map.delete name'
                                                           (Map.filter (\ pairs ->
                                                                            let declared = Set.fromList (concatMap (symbols . fst) pairs) in
                                                                            not (Set.null (Set.intersection declared referenced))) declMap))
              header =
                  foldModule (\ s r -> r <> s)
                             (\ _p pref s suff r -> r <> pref <> s <> suff)
                             (\ _n pref _ suff r -> r <> pref <> modName <> suff)
                             (\ _w pref s suff r -> r <> pref <> s <> suff)
                             (\ _ r -> r)
                             (\ _e _ _ _ r -> r)
                             (\ _ r -> r)
                             (\ _i _ _ _ r -> r)
                             (\ _d _ _ _ r -> r)
                             (\ _ r -> r)
                             m text ""
              exportsPrefix = fromMaybe "" $
                  foldModule (\ _ r -> r)
                             (\ _p _ _ _ r -> r)
                             (\ _n _ _ _ r -> r)
                             (\ _w _ _ _ r -> r)
                             (\ _ r -> r)
                             (\ _e pref _ _ r -> (maybe (Just pref) Just r))
                             (\ _ r -> r)
                             (\ _i _ _ _ r -> r)
                             (\ _d _ _ _ r -> r)
                             (\ _ r -> r)
                             m text Nothing
              importsPrefix = fromMaybe "" $
                  foldModule (\ _ r -> r)
                             (\ _p _ _ _ r -> r)
                             (\ _n _ _ _ r -> r)
                             (\ _w _ _ _ r -> r)
                             (\ _ r -> r)
                             (\ _e _ _ _ r -> r)
                             (\ _ r -> r)
                             (\ _i pref _ _ r -> maybe (Just pref) Just r)
                             (\ _d _ _ _ r -> r)
                             (\ _ r -> r)
                             m text Nothing
              declPrefix = fromMaybe "" $
                  foldModule (\ _ r -> r)
                             (\ _p _ _ _ r -> r)
                             (\ _n _ _ _ r -> r)
                             (\ _w _ _ _ r -> r)
                             (\ _ r -> r)
                             (\ _e _ _ _ r -> r)
                             (\ _ r -> r)
                             (\ _i _ _ _ r -> r)
                             (\ _d pref _ _ r -> maybe (Just pref) Just r)
                             (\ _ r -> r)
                             m text Nothing
              -- Grab the old imports
              oldImports = fromMaybe "" $
                  foldModule (\ _ r -> r)
                             (\ _p _ _ _ r -> r)
                             (\ _n _ _ _ r -> r)
                             (\ _w _ _ _ r -> r)
                             (\ _ r -> r)
                             (\ _e _ _ _ r -> r)
                             (\ _ r -> r)
                             (\ _i pref s suff r -> Just (maybe (s <> suff) (\ l -> l <> pref <> s <> suff) r))
                             (\ _d _ _ _ r -> r)
                             (\ _ r -> r)
                             m text Nothing
        newReExporter =
            header <> oldImports <> "\n" <> unlines (List.map (prettyPrintWithMode defaultMode) (elems (mapWithKey toImportDecl declMap)))
            where
              header =
                    foldModule (\ s r -> r <> s)
                               (\ _p pref s suff r -> r <> pref <> s <> suff)
                               (\ _n pref s suff r -> r <> pref <> s <> suff)
                               (\ _w pref s suff r -> r <> pref <> s <> suff)
                               (\ s r -> r <> s)
                               (\ _e pref s suff r -> r <> pref <> s <> suff)
                               (\ s r -> r <> s)
                               (\ _i _ _ _ r -> r)
                               (\ _d _ _ _ r -> r)
                               (\ _ r -> r)
                               m text ""
              oldImports = fromMaybe "" $
                    foldModule (\ _ r -> r)
                               (\ _p _ _ _ r -> r)
                               (\ _n _ _ _ r -> r)
                               (\ _w _ _ _ r -> r)
                               (\ _ r -> r)
                               (\ _e _ _ _ r -> r)
                               (\ _ r -> r)
                               (\ _i pref s suff r -> maybe (Just (pref <> s <> suff)) (Just . (<> (pref <> s <> suff))) r)
                               (\ _d _ _ _ r -> r)
                               (\ _ r -> r)
                               m text Nothing

        -- Build a map from module name to the list of declarations that will be in that module.
        declMap :: Map S.ModuleName [(A.Decl SrcSpanInfo, String)]
        declMap =
            foldModule (\ _ r -> r)
                       (\ _ _ _ _ r -> r)
                       (\ _ _ _ _ r -> r)
                       (\ _ _ _ _ r -> r)
                       (\ _ r -> r)
                       (\ _ _ _ _ r -> r)
                       (\ _ r -> r)
                       (\ _ _ _ _ r -> r)
                       (\ d pref s suff r -> foldr (\ sym mp -> insertWith (++) (subModuleName (sModuleName moduleName) sym) [(d, pref <> s <> suff)] mp) r (symbols d))
                       (\ _ r -> r)
                       m text Map.empty
splitModule' _ _ _ = error "splitModule'"

-- | Build an import of the symbols created by a declaration.
toImportDecl :: S.ModuleName -> [(A.Decl SrcSpanInfo, String)] -> S.ImportDecl
toImportDecl (S.ModuleName modName) decls =
    S.ImportDecl {S.importLoc = def,
                  S.importModule = S.ModuleName modName,
                  S.importQualified = False,
                  S.importSrc = False,
                  S.importPkg = Nothing,
                  S.importAs = Nothing,
                  S.importSpecs = Just (False, (nub (concatMap toImportSpecs decls)))}
    where
      toImportSpecs :: (A.Decl SrcSpanInfo, String) -> [S.ImportSpec]
      toImportSpecs = imports . fst

-- | Build export specs of the symbols created by a declaration.
toExportSpecs :: (A.Decl SrcSpanInfo, String) -> [S.ExportSpec]
toExportSpecs (x, _) = exports x -- List.map (\ name -> A.EVar def (A.UnQual def name)) (mapNames (symbols x))

tests :: Test
tests = TestList [test1]

test1 :: Test
test1 =
    TestCase $
      do _ <- system "rsync -aHxS --delete testdata/original/ testdata/copy"
         runCleanT . noisily $
           do modifyParams (\ p -> p {sourceDirs = ["testdata/copy"]})
              splitModule (S.ModuleName "Debian.Repo.Package")
         (code, out, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/splitresult", "testdata/copy"] ""
         let out' = unlines (List.filter (not . isPrefixOf "Only") (lines out))
         assertEqual "splitModule" (ExitFailure 1, "", "") (code, out', err)
