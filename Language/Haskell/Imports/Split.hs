{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Split
    ( splitModule
    , tests
    ) where

import Control.Exception (throw)
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Default (Default(def))
import Data.List as List (filter, nub, isPrefixOf, intercalate, map)
import Data.Map as Map (delete, elems, empty, filter, insert, insertWith, keys, Map, mapKeys, mapWithKey, toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (fromList, intersection, null, Set, map)
import Data.Set.Extra (gFind)
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(ParseOk, ParseFailed))
import qualified Language.Haskell.Exts.Annotated as A
import qualified Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
-- import Language.Haskell.Exts.Syntax (Decl, ExportSpec(EVar), ImportDecl(..), ImportSpec(IVar), Module(..), ModuleName(..), Name(..), QName(UnQual))
import Language.Haskell.Imports.Clean (cleanImports)
import Language.Haskell.Imports.Common (withCurrentDirectory, symbols, ModuleName, Decl, ExportSpec, ImportDecl, ImportSpec, Module, ModuleName, Name)
import Language.Haskell.Imports.Fold (foldModule)
import Language.Haskell.Imports.Params (runParamsT)
import System.Cmd (system)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath ((<.>), dropExtension)
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual, Test(TestCase, TestList))

subModuleName :: A.ModuleName a -> A.Name b -> A.ModuleName a
subModuleName (A.ModuleName l moduleName) name =
    A.ModuleName l (case name of
                      A.Symbol _ s -> moduleName <.> f s
                      A.Ident _ s -> moduleName <.> f s)
    where
      f x =
          case List.filter isAlphaNum x of
            [] -> "OtherSymbols"
            (c : s) -> if isAlpha c then toUpper c : s else throw $ userError $ "symbolToModuleName: must begin with a letter: " ++ show x

-- | Split each of a module's declarations into a new module.  The
-- resulting modules may need to import some of the other split
-- modules, but we don't know which or how to avoid circular imports,
-- so a commented out list of imports is added.
splitModule :: FilePath -> IO ()
splitModule path =
    do parsed <- A.parseFileWithComments defaultParseMode path
       text <- readFile path
       let newFiles = splitModule' path parsed text
       -- Create a subdirectory named after the old module
       createDirectoryIfMissing True (dropExtension path)
       -- Write the new modules
       mapM_ (uncurry writeFile) (Map.toList newFiles)
       -- Clean the new modules
       runParamsT "dist/scratch" $ mapM_ cleanImports (Map.keys newFiles)

-- | If the original module was M, the the split operation creates a
-- subdirectory M containing a module for each declaration of the
-- original module, and replaces M.hs with a module that imports each
-- of the split declarations that were originally exported.
splitModule' :: FilePath -> ParseResult (A.Module SrcSpanInfo, [Comment]) -> String -> Map FilePath String
splitModule' _ (ParseOk (A.Module _ _ _ _ [], _)) _ = empty -- No declarations - nothing to split
splitModule' _ (ParseOk (A.Module _ _ _ _ [_], _)) _ = empty -- One declaration - nothing to split
splitModule' path (ParseFailed _ _) _ = throw $ userError $ "Parse of " ++ path ++ " failed"
splitModule' path (ParseOk (A.Module _ Nothing _ _ _, _)) _ = throw $ userError $ "splitModule: " ++ path ++ " has no explicit header"
splitModule' path (ParseOk (A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _, _)) _ = throw $ userError $ "splitModule: " ++ path ++ " has no explicit export list"
splitModule' path (ParseOk (m@(A.Module _ (Just (A.ModuleHead _ moduleName _ (Just _))) _ _ _), _comments)) text =
    Map.insert path newReExporter $ newModules
    where
        -- Build the new modules
        newModules :: Map FilePath String
        newModules = mapKeys (\ (A.ModuleName _ modName) -> List.map (\ c -> case c of '.' -> '/'; x -> x) modName <.> "hs") $
                     mapWithKey newModule declPairs
        newModule name@(A.ModuleName _ modName) modDecls =
            let newExports = nub (concatMap toExportSpecs modDecls) in
            -- In this module, we need to import any module that declares a symbol
            -- referenced here.
            let referenced = Set.map voidName (gFind modDecls :: Set (A.Name SrcSpanInfo)) in
            let newImports :: Map ModuleName ImportDecl
                newImports = mapWithKey toImportDecl (Map.delete name
                                                             (Map.filter (\ pairs ->
                                                                              let declared = Set.fromList (concatMap (symbols . fst) pairs) in
                                                                              not (Set.null (Set.intersection declared referenced))) declPairs)) in
            let (r, x, y, z) =
                    foldModule (\ _p pre s (r, x, y, z) -> (r <> pre <> s, x, y, z))
                               (\ _n pre s (r, x, y, z) -> (r <> pre <> modName, x, y, z))
                               (\ _w pre s (r, x, y, z) -> (r <> pre <> s, x, y, z))
                               (\ _e pre s (r, x, y, z) -> (r, maybe (Just pre) Just x, y, z))
                               (\ _i pre s (r, x, y, z) -> (r, x, maybe (Just pre) Just y, z))
                               (\ _d pre s (r, x, y, z) -> (r, x, y, maybe (Just pre) Just z))
                               (\ _ r -> r)
                               m text ("", Nothing, Nothing, Nothing) in
            r <> fromMaybe "" x <> intercalate ", " (nub (List.map (prettyPrintWithMode defaultMode) newExports)) <> fromMaybe "" y <> unlines (List.map (prettyPrintWithMode defaultMode) (elems newImports)) <> oldImports <> fromMaybe "" z <> concatMap snd (reverse modDecls) <> "\n"
        -- Grab the old imports
        oldImports = case foldModule (\ _p _ _ r -> r)
                                     (\ _n _ _ r -> r)
                                     (\ _w _ _ r -> r)
                                     (\ _e _ _ r -> r)
                                     (\ _i pre s r -> r ++ [pre, s])
                                     (\ _d _ _ r -> r)
                                     (\ _ r -> r)
                                     m text [] of
                       (_ : xs) -> concat xs
                       [] -> ""
        newReExporter =
            let (r, x) =
                    foldModule (\ _p pre s (r, x) -> (r <> pre <> s, x))
                               (\ _n pre s (r, x) -> (r <> pre <> s, x))
                               (\ _w pre s (r, x) -> (r <> pre <> s, x))
                               (\ _e pre s (r, x) -> (r <> pre <> s, x))
                               (\ _i pre _ (r, x) -> (r, maybe (Just pre) Just x))
                               (\ _d _ _ r -> r)
                               (\ _ r -> r)
                               m text ("", Nothing) in
            r <> fromMaybe "" x <> unlines (List.map (prettyPrintWithMode defaultMode) allNewImports)
        allNewImports :: [ImportDecl]
        allNewImports = elems (mapWithKey toImportDecl declPairs)
        -- Build a (new module name, declaration list) map
        declPairs :: Map ModuleName [(Decl, String)]
        declPairs =
            foldModule (\ _ _ _ r -> r)
                       (\ _ _ _ r -> r)
                       (\ _ _ _ r -> r)
                       (\ _ _ _ r -> r)
                       (\ _ _ _ r -> r)
                       (\ d pre s r -> foldr (\ name mp -> insertWith (++) (subModuleName moduleName name) [(d, pre <> s)] mp) r (symbols d))
                       (\ _ r -> r)
                       m text Map.empty
        -- Grab any comment before the module header
        oldHeader = foldModule (\ _ pre s r -> r <> pre <> s)
                               (\ _ pre _ r -> r <> pre)
                               (\ _ _ _ r -> r)
                               (\ _ _ _ r -> r)
                               (\ _ _ _ r -> r)
                               (\ _ _ _ r -> r)
                               (\ _ r -> r)
                               m text ""

-- | Build an import of the symbols created by a declaration.
toImportDecl :: ModuleName -> [(Decl, String)] -> ImportDecl
toImportDecl modName decls =
    A.ImportDecl {A.importAnn = def,
                  A.importModule = modName,
                  A.importQualified = False,
                  A.importSrc = False,
                  A.importPkg = Nothing,
                  A.importAs = Nothing,
                  A.importSpecs = Just (A.ImportSpecList def False (nub (concatMap toImportSpecs decls)))}
    where
      toImportSpecs :: (Decl, String) -> [ImportSpec]
      toImportSpecs = List.map (A.IVar def) . mapNames . symbols . fst

voidName (A.Ident a x) = A.Ident () x
voidName (A.Symbol a x) = A.Symbol () x

mapNames [] = []
mapNames (A.Ident () x : more) = A.Ident def x : mapNames more
mapNames (A.Symbol () x : more) = A.Symbol def x : mapNames more

-- | Build export specs of the symbols created by a declaration.
toExportSpecs :: (Decl, String) -> [ExportSpec]
toExportSpecs (x, _) = List.map (\ name -> A.EVar def (A.UnQual def name)) (mapNames (symbols x))

tests :: Test
tests = TestList [test1]

test1 :: Test
test1 =
    TestCase
      (system "rsync -aHxS --delete testdata/original/ testdata/copy" >>
       withCurrentDirectory "testdata/copy"
         (splitModule "Debian/Repo/Package.hs" >>= \ () ->
          readProcessWithExitCode "diff" ["-ru", "../splitresult", "."] "" >>= \ (code, out, err) ->
          assertEqual
            "splitModule"
            (ExitFailure 1, "", "")
            (code, unlines (List.filter (not . isPrefixOf "Only") (lines out)), err)))
