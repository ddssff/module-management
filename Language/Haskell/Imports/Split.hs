{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Split
    ( splitModule
    , tests
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try)
import Control.Monad.Trans (liftIO)
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Default (def)
import Data.List as List (filter, nub)
import Data.Map as Map (delete, elems, empty, filter, fromList, insert, insertWith, keys, Map, mapKeys, mapWithKey, toList)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>))
import Data.Set as Set (fromList, intersection, null, Set)
import Data.Set.Extra (gFind)
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(ParseOk))
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrintWithMode)
import Language.Haskell.Exts.Syntax (Decl, ExportSpec(EVar), ImportDecl(..), ImportSpec(IVar), Module(..), ModuleName(..), Name(..), QName(UnQual))
import Language.Haskell.Imports.Clean (cleanImports)
import Language.Haskell.Imports.Fold (foldModule)
import Language.Haskell.Imports.Params (runParamsT)
import Language.Haskell.Imports.Syntax (nameString, symbol)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import System.FilePath ((<.>), dropExtension)
import Test.HUnit (assertEqual, Test(TestCase, TestList))

-- Should be QName -> Name -> QName or something
subModuleName :: ModuleName -> Name -> ModuleName
subModuleName (ModuleName moduleName) name =
    ModuleName (case name of
                  Symbol name -> moduleName <.> f name
                  Ident name -> moduleName <.> f name)
    where
      f x =
          case List.filter isAlphaNum x of
            [] -> "OtherSymbols"
            (c : s) -> if isAlpha c then toUpper c : s else error $ "symbolToModuleName: must begin with a letter: " ++ show x

-- | Split each of a module's declarations into a new module.  The
-- resulting modules may need to import some of the other split
-- modules, but we don't know which or how to avoid circular imports,
-- so a commented out list of imports is added.
splitModule :: FilePath -> IO ()
splitModule path =
    do source <- liftIO $ try ((,) <$> parseFileWithComments defaultParseMode path <*> readFile path)
       case source of
         Left (e :: SomeException) -> error (path ++ ": " ++ show e)
         Right (ParseOk (m@(Module loc moduleName pragmas warn (Just exports) imports decls), comments), text) ->
             case decls of
               [] -> error "splitModule: module only no declarations"
               [x] -> error "splitModule: module only has one declaration"
               xs -> do let -- Build a (new module name, declaration list) map
                            declPairs :: Map ModuleName [(Decl, String)]
                            declPairs =
                                foldl ins Map.empty (foldModule
                                                           (\ _ _ _ _ r -> r)
                                                           (\ _ _ _ _ r -> r)
                                                           (\ x pre s sp r -> (symbol x, (x, maybe "" fst pre <> s)) : r)
                                                           (\ _ _ r -> r)
                                                           m comments text [])
                            ins :: Map ModuleName [a] -> (Maybe Name, a) -> Map ModuleName [a]
                            ins mp (Just name, x) = insertWith (++) (subModuleName moduleName name) [x] mp
                            ins mp (Nothing, x) = error "splitModule: no symbol"


                            -- Map from the symbol we will declare to the module name that will
                            -- contain it.  If a symbol used in a declaration is mentioned in
                            -- this map we need to import the corresponding module.
                            declSymbols :: Map Name ModuleName
                            declSymbols = Map.fromList (concatMap (\ (moduleName, declPairs) -> map (\ (decl, _) -> (fromJust (symbol decl), moduleName)) declPairs) (Map.toList declPairs))

                            allNewImports :: Map ModuleName ImportDecl
                            allNewImports = mapWithKey toImportDecl declPairs
                            -- allNewImports = mapWithKey  declPairs
                            -- Grab any comment before the module header
                            oldHeader = foldModule (\ _ pre _ _ r -> r <> maybe "" fst pre) (\ _ _ _ _ r -> r) (\ _ _ _ _ r -> r) (\ s _ r -> r <> s) m comments text ""
                            -- Grab the old imports
                            oldImports = foldModule (\ _ _ _ _ r -> r) (\ _ pre s _ r -> r <> maybe "" fst pre <> s) (\ _ _ _ _ r -> r) (\ _ _ r -> r) m comments text ""
                            -- Build the new modules
                            newModules :: Map ModuleName String
                            newModules = mapWithKey (\ mod decls ->
                                                         let newExports = catMaybes (map toExportSpec decls) in
                                                         -- In this module, we need to import any module that declares a symbol
                                                         -- referenced here.
                                                         let referenced = gFind decls :: Set Name in
                                                         let newImports = mapWithKey toImportDecl (Map.delete mod
                                                                                                   (Map.filter (\ pairs ->
                                                                                                                    let declared = Set.fromList (map (fromJust . symbol . fst) pairs) in
                                                                                                                    not (Set.null (Set.intersection declared referenced))) declPairs)) in
                                                         oldHeader <>
                                                         prettyPrintWithMode defaultMode (Module
                                                                                              loc
                                                                                              mod
                                                                                              pragmas
                                                                                              warn
                                                                                              (Just newExports)
                                                                                              (elems newImports)
                                                                                              []) <>
                                                         "\n\n" <>
                                                         oldImports <>
                                                         concatMap snd decls) declPairs
                            -- Replace the body of the old module with imports
                            newReExporter = oldHeader <> prettyPrintWithMode defaultMode (Module loc (subModuleName moduleName (Symbol "ReExporter")) pragmas warn (Just exports) (elems allNewImports) [])
                            -- The paths and contents of the files we will write
                            newFiles :: Map FilePath String
                            newFiles =
                                Map.insert (dropExtension path <.> "hs") newReExporter $
                                Map.mapKeys (\ (ModuleName mod) -> map (\ c -> case c of '.' -> '/'; c -> c) mod <.> "hs") newModules
                        -- Create a subdirectory named after the old module
                        createDirectoryIfMissing True (dropExtension path)
                        -- Write the new modules
                        mapM_ (uncurry writeFile) (Map.toList newFiles)
                        -- Clean the new modules
                        mapM_ (runParamsT . cleanImports) (Map.keys newFiles)

exported :: Module -> String -> Bool
exported (Module _ _ _ _ exports _ _) s =
    maybe True (exported' s) exports
    where
      exported' :: String -> [ExportSpec] -> Bool
      exported' s exports =
          any (== (Just s)) (map (fmap nameString . symbol) exports)

toImportDecl :: ModuleName -> [(Decl, String)] -> ImportDecl
toImportDecl mod decls =
    ImportDecl {importLoc = def,
                importModule = mod,
                importQualified = False,
                importSrc = False,
                importPkg = Nothing,
                importAs = Nothing,
                importSpecs = Just (False, nub (map toImportSpec decls))}

toImportSpec :: (Decl, String) -> ImportSpec
toImportSpec = IVar . fromJust . symbol . fst

mkImport :: ImportSpec -> ModuleName -> ImportDecl
mkImport sym mod =
    ImportDecl {importLoc = def,
                importModule = mod,
                importQualified = False,
                importSrc = False,
                importPkg = Nothing,
                importAs = Nothing,
                importSpecs = Just (False, [sym])}

toExportSpec :: (Decl, String) -> Maybe ExportSpec
toExportSpec (x, _) =
    case symbol x of
      Just name -> Just (EVar (UnQual name))
      Just name -> Just (EVar (UnQual name))
      Nothing -> Nothing

tests :: Test
tests = TestList [test1]

test1 :: Test
test1 =
    TestCase
      (setCurrentDirectory "testdata" >>
       splitModule "Debian/Repo/Package.hs" >>= \ result ->
       assertEqual
         "splitModule"
         ()
         result)
