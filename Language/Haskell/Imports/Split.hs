{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Split
    ( splitModule
    , tests
    ) where

import Control.Exception (throw)
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Default (def)
import Data.List as List (filter, nub)
import Data.Map as Map (delete, elems, empty, filter, insert, insertWith, keys, Map, mapKeys, mapWithKey, toList)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>))
import Data.Set as Set (fromList, intersection, null, Set)
import Data.Set.Extra (gFind)
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(ParseOk, ParseFailed))
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrintWithMode)
import Language.Haskell.Exts.Syntax (Decl, ExportSpec(EVar), ImportDecl(..), ImportSpec(IVar), Module(..), ModuleName(..), Name(..), QName(UnQual))
import Language.Haskell.Imports.Clean (cleanImports)
import Language.Haskell.Imports.Common (withCurrentDirectory)
import Language.Haskell.Imports.Fold (foldModule)
import Language.Haskell.Imports.Params (runParamsT)
import Language.Haskell.Imports.Syntax (symbol)
import System.Cmd (system)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), dropExtension)
import Test.HUnit (assertEqual, Test(TestCase, TestList))

-- Should be QName -> Name -> QName or something
subModuleName :: ModuleName -> Name -> ModuleName
subModuleName (ModuleName moduleName) name =
    ModuleName (case name of
                  Symbol s -> moduleName <.> f s
                  Ident s -> moduleName <.> f s)
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
    do parsed <- parseFileWithComments defaultParseMode path
       text <- readFile path
       let newFiles = splitModule' path parsed text
       -- Create a subdirectory named after the old module
       createDirectoryIfMissing True (dropExtension path)
       -- Write the new modules
       mapM_ (uncurry writeFile) (Map.toList newFiles)
       -- Clean the new modules
       runParamsT "dist/scratch" $ mapM_ cleanImports (Map.keys newFiles)

splitModule' :: FilePath -> ParseResult (Module, [Comment]) -> String -> Map FilePath String
splitModule' _ (ParseOk (Module _ _ _ _ _ _ [], _)) _ = empty -- No declarations - nothing to split
splitModule' _ (ParseOk (Module _ _ _ _ _ _ [_], _)) _ = empty -- One declaration - nothing to split
splitModule' path (ParseFailed _ _) _ = throw $ userError $ "Parse of " ++ path ++ " failed"
splitModule' path (ParseOk (Module _ _ _ _ Nothing _ _, _)) _ =
    throw $ userError $ "splitModule: " ++ path ++ " has no explicit export list"
splitModule' path (ParseOk (m@(Module loc moduleName pragmas warn (Just exports) _imports _decls), comments)) text =
    Map.insert path newReExporter $
    Map.mapKeys (\ (ModuleName modName) -> map (\ c -> case c of '.' -> '/'; x -> x) modName <.> "hs") newModules
    where
        -- Build a (new module name, declaration list) map by looking at the
        -- import list of the argument module.
        declPairs :: Map ModuleName [(Decl, String)]
        declPairs =
            foldl ins Map.empty (foldModule
                                       (\ _ _ _ _ r -> r)
                                       (\ _ _ _ _ r -> r)
                                       (\ x pre s _sp r -> (symbol x, (x, maybe "" fst pre <> s)) : r)
                                       (\ _ _ r -> r)
                                       m comments text [])
        ins :: Map ModuleName [a] -> (Maybe Name, a) -> Map ModuleName [a]
        ins mp (Just name, x) = insertWith (++) (subModuleName moduleName name) [x] mp
        ins _mp (Nothing, _x) = throw $ userError $ "splitModule: no symbol"
        allNewImports :: Map ModuleName ImportDecl
        allNewImports = mapWithKey toImportDecl declPairs
        -- Grab any comment before the module header
        oldHeader = foldModule (\ _ pre _ _ r -> r <> maybe "" fst pre) (\ _ _ _ _ r -> r) (\ _ _ _ _ r -> r) (\ s _ r -> r <> s) m comments text ""
        -- Grab the old imports
        oldImports = foldModule (\ _ _ _ _ r -> r) (\ _ pre s _ r -> r <> maybe "" fst pre <> s) (\ _ _ _ _ r -> r) (\ _ _ r -> r) m comments text ""
        -- Build the new modules
        newModules :: Map ModuleName String
        newModules = mapWithKey (\ modName modDecls ->
                                     let newExports = nub $ catMaybes (map toExportSpec modDecls) in
                                     -- In this module, we need to import any module that declares a symbol
                                     -- referenced here.
                                     let referenced = gFind modDecls :: Set Name in
                                     let newImports = mapWithKey toImportDecl (Map.delete modName
                                                                               (Map.filter (\ pairs ->
                                                                                                let declared = Set.fromList (map (fromJust . symbol . fst) pairs) in
                                                                                                not (Set.null (Set.intersection declared referenced))) declPairs)) in
                                     oldHeader <>
                                     prettyPrintWithMode defaultMode (Module
                                                                          loc
                                                                          modName
                                                                          pragmas
                                                                          warn
                                                                          (Just newExports)
                                                                          (elems newImports)
                                                                          []) <>
                                     "\n\n" <>
                                     oldImports <>
                                     concatMap snd modDecls) declPairs
        -- Build the the module that replaces
        -- the original argument.  Replace the
        -- body of the old module with imports.
        newReExporter = oldHeader <> prettyPrintWithMode defaultMode
                                       (Module loc (subModuleName moduleName (Symbol "ReExporter"))
                                            pragmas warn (Just exports) (elems allNewImports) [])

toImportDecl :: ModuleName -> [(Decl, String)] -> ImportDecl
toImportDecl modName decls =
    ImportDecl {importLoc = def,
                importModule = modName,
                importQualified = False,
                importSrc = False,
                importPkg = Nothing,
                importAs = Nothing,
                importSpecs = Just (False, nub (map toImportSpec decls))}

toImportSpec :: (Decl, String) -> ImportSpec
toImportSpec = IVar . fromJust . symbol . fst

toExportSpec :: (Decl, String) -> Maybe ExportSpec
toExportSpec (x, _) =
    case symbol x of
      Just name -> Just (EVar (UnQual name))
      Nothing -> Nothing

tests :: Test
tests = TestList [test1]

test1 :: Test
test1 =
    TestCase
      (system "rsync -aHxS --delete testdata/original/ testdata/copy" >>
       withCurrentDirectory "testdata/copy"
         (splitModule "Debian/Repo/Package.hs" >>= \ () ->
          readFile "Debian/Repo/Package.hs" >>= \ result ->
          assertEqual
            "splitModule"
            (unlines
             ["{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}",
              "{-# OPTIONS -fno-warn-name-shadowing  #-}",
              "module Debian.Repo.Package.ReExporter",
              "       (sourceFilePaths, binaryPackageSourceVersion, binarySourceVersion,",
              "        sourcePackageBinaryNames, sourceBinaryNames, toSourcePackage,",
              "        toBinaryPackage, binaryPackageSourceID, sourcePackageBinaryIDs,",
              "        sourcePackagesOfIndex, sourcePackagesOfCachedIndex,",
              "        binaryPackagesOfIndex, binaryPackagesOfCachedIndex, getPackages,",
              "        putPackages, releaseSourcePackages, releaseBinaryPackages)",
              "       where",
              "import Debian.Repo.Package.BinaryPackageSourceID (binaryPackageSourceID)",
              "import Debian.Repo.Package.BinaryPackageSourceVersion (binaryPackageSourceVersion)",
              "import Debian.Repo.Package.BinaryPackagesOfCachedIndex (binaryPackagesOfCachedIndex)",
              "import Debian.Repo.Package.BinaryPackagesOfIndex (binaryPackagesOfIndex)",
              "import Debian.Repo.Package.BinarySourceVersion (binarySourceVersion)",
              "import Debian.Repo.Package.GetPackages (getPackages)",
              "import Debian.Repo.Package.PutPackages (putPackages)",
              "import Debian.Repo.Package.ReleaseBinaryPackages (releaseBinaryPackages)",
              "import Debian.Repo.Package.ReleaseSourcePackages (releaseSourcePackages)",
              "import Debian.Repo.Package.SourceBinaryNames (sourceBinaryNames)",
              "import Debian.Repo.Package.SourceFilePaths (sourceFilePaths)",
              "import Debian.Repo.Package.SourcePackageBinaryIDs (sourcePackageBinaryIDs)",
              "import Debian.Repo.Package.SourcePackageBinaryNames (sourcePackageBinaryNames)",
              "import Debian.Repo.Package.SourcePackagesOfCachedIndex (sourcePackagesOfCachedIndex)",
              "import Debian.Repo.Package.SourcePackagesOfIndex (sourcePackagesOfIndex)",
              "import Debian.Repo.Package.ToBinaryPackage (toBinaryPackage)",
              "import Debian.Repo.Package.ToSourcePackage (toSourcePackage)",
              ""])
          result))
