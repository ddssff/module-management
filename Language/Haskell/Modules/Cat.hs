{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Cat
    ( catModules
    , test1
    , test2
    ) where

import Control.Applicative ((<$>))
import Control.Exception (throw)
import Control.Monad as List (filterM, mapM, mapM_)
import Control.Monad.Trans (liftIO)
import Data.Default (def)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.List as List (filter, intercalate, isPrefixOf, map, null)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, (<>))
import Data.Set as Set (fromList, map, member, Set, toList, union)
import Language.Haskell.Exts.Annotated (defaultParseMode, parseFileWithComments)
import Language.Haskell.Exts.Annotated.Simplify (sDecl, sExportSpec, sModuleName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ExportSpecList(ExportSpecList), ImportDecl(importModule), Module(Module), ModuleHead(ModuleHead), ModuleName(ModuleName))
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(EModuleContents), ModuleName(..))
import Language.Haskell.Exts.Pretty (defaultMode, prettyPrintWithMode)
import Language.Haskell.Modules.Common (checkParse, Module, modulePath, removeFileIfPresent, replaceFileIfDifferent, withCurrentDirectory)
import Language.Haskell.Modules.Fold (foldModule)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Params (MonadClean, runCleanT)
import System.Cmd (system)
import System.Exit (ExitCode(ExitFailure))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual, Test(TestCase))

-- | Merge the declarations from several modules into a single new
-- one.  Note that a circular imports can be created by this
-- operation, in which case you will have to add more modules to the
-- merge.
catModules :: MonadClean m => Set S.ModuleName -> [S.ModuleName] -> S.ModuleName -> m ()
catModules univ old new = liftIO (catModulesIO univ old new) >>= List.mapM_ cleanImports . List.map modulePath . Set.toList

catModulesIO :: Set S.ModuleName -> [S.ModuleName] -> S.ModuleName -> IO (Set S.ModuleName)
catModulesIO univ old new
    | List.null old = throw $ userError "catModules: invalid argument"
    | elem new old = throw $ userError "catModules: invalid destination"
    | True = do let univ' = union univ (Set.fromList old)
                old' <- List.mapM (\ name -> do text <- readFile (modulePath name)
                                                (m, _) <- checkParse name <$> parseFileWithComments defaultParseMode (modulePath name)
                                                return (name, m, text)) old
                -- Generate the modified modules
                changed <- filterM (doModule old' new) (Set.toList univ') >>=
                           -- The first from module turned into the new
                           -- module, the other from modules disappeared.
                           return . Set.map (\ x -> if elem x old then new else x) . Set.fromList
                -- Remove the original modules
                List.mapM_ (removeFileIfPresent . modulePath) old
                return changed

-- Update the module 'name' to reflect the result of the cat operation.
doModule :: [(S.ModuleName, Module, String)] -> S.ModuleName -> S.ModuleName -> IO Bool
doModule old@((first, _, _) : _ ) new name =
          do text <- readFile . modulePath $ name
             (m, _) <- checkParse name <$> parseFileWithComments defaultParseMode (modulePath name)
             let name' = if name == first then new else name
                 text' = catModules'' old new (name, m, text)
             replaceFileIfDifferent (modulePath name') text'
doModule [] _ _ = error "catModulesIO"

catModules'' :: [(S.ModuleName, Module, String)] -> S.ModuleName -> (S.ModuleName, Module, String) -> String
catModules'' [] _ _ = error "catModules''"
catModules'' from@((first, _, _) : _) to (name, m, text) =
    case () of
      _ | name == first -> doFirst from to (m, text)
        | not (elem name (to : fromNames)) -> doOther from to (m, text)
        | True -> ""
    where
      fromNames = List.map (\ (x, _, _) -> x) from

doFirst :: [(S.ModuleName, Module, String)] -> S.ModuleName -> (Module, String) -> String
doFirst old new (m, text) =
    header ++ fromMaybe "" exports ++ fromMaybe "" imports ++ fromMaybe "" decls
    where
      header =
          foldModule echo
                     (\ _ pre _ r -> r <> pre <> prettyPrintWithMode defaultMode new)
                     echo
                     ignore
                     ignore
                     ignore
                     (\ _ r -> r)
                     m text ""
      exports =
          foldModule ignore
                     ignore
                     ignore
                     (\ _e pre _ r -> Just (fromMaybe (pre <> maybe "" (intercalate ", " . List.map (prettyPrintWithMode defaultMode)) (mergeExports old new)) r))
                     ignore
                     ignore
                     (\ _ r -> r)
                     m text Nothing
      imports =
          foldModule ignore
                     ignore
                     ignore
                     ignore
                     (\ _i pre _ r -> Just (fromMaybe (pre <> unlines (List.map (moduleImports oldNames) old)) r))
                     ignore
                     (\ _ r -> r)
                     m text Nothing
      decls =
          foldModule ignore
                     ignore
                     ignore
                     ignore
                     ignore
                     (\ _d _ _ r -> Just (fromMaybe (unlines (List.map (moduleDecls (fromList oldNames) new) old)) r))
                     (\ s r -> Just (maybe s (<> s) r))
                     m text Nothing

      oldNames = List.map (\ (x, _, _) -> x) old

echo :: Monoid m => t -> m -> m -> m -> m
echo _ pre s r = r <> pre <> s

ignore :: t -> t1 -> t2 -> t3 -> t3
ignore _ _ _ r = r

doOther :: [(S.ModuleName, Module, String)] -> S.ModuleName -> (Module, String) -> String
doOther old new@(S.ModuleName new') (m, text) =
    header ++ exports ++ imports ++ decls
    where
      header = foldModule echo echo echo ignore ignore ignore (\ _ r -> r) m text ""
      exports = foldModule ignore ignore ignore fixModuleExport ignore ignore (\ _ r -> r) m text ""
      imports =
          foldModule ignore
                     ignore
                     ignore
                     ignore
                     (\ x pre s r ->
                          r <> pre <> (if elem (sModuleName (A.importModule x)) oldNames
                                       then prettyPrintWithMode defaultMode (x {A.importModule = A.ModuleName def new'})
                                       else s))
                     ignore
                     (\ _ r -> r)
                     m text ""
      decls =
          foldModule ignore
                     ignore
                     ignore
                     ignore
                     ignore
                     echo
                     (\ s r -> r <> s)
                     m text ""

      fixModuleExport x pre s r =
          r <> case sExportSpec x of
                 S.EModuleContents y
                     | elem y oldNames ->
                         "\n     , " <> prettyPrintWithMode defaultMode (S.EModuleContents new)
                 _ -> pre <> s

      oldNames = List.map (\ (x, _, _) -> x) old

mergeExports :: [(S.ModuleName, Module, String)] -> S.ModuleName -> Maybe [S.ExportSpec]
mergeExports old new =
    Just (concatMap mergeExports' old)
    where
      mergeExports' (_, A.Module _ Nothing _ _ _, _) = error "catModules: no explicit export list"
      mergeExports' (_, A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _, _) = error "catModules: no explicit export list"
      mergeExports' (_, A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList _ e)))) _ _ _, _) = updateModuleContentsExports oldNames new (List.map sExportSpec e)
      mergeExports' (_, _, _) = error "mergeExports'"
      oldNames = List.map (\ (x, _, _) -> x) old

updateModuleContentsExports :: [S.ModuleName] -> S.ModuleName -> [S.ExportSpec] -> [S.ExportSpec]
updateModuleContentsExports old new es =
    foldl f [] es
    where
      f :: [S.ExportSpec] -> S.ExportSpec ->  [S.ExportSpec]
      f ys (S.EModuleContents m) =
          let e' = S.EModuleContents (if elem m old then new else m) in
          if elem e' ys then ys else e' : ys
      f ys e = e : ys

moduleImports :: [S.ModuleName] -> (S.ModuleName, Module, String) -> String
moduleImports old' (_, m, text) =
    foldModule (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ x pre s r ->
                    r
                    -- If this is the first import, omit the prefix, it includes the ") where" text.
                    <> (if r == "" then "" else pre)
                    <> if elem (sModuleName (A.importModule x)) old' then "" else s)
               (\ _ _ _ r -> r)
               (\ _ r -> r)
               m text "" <> "\n"

-- | Grab the declarations out of the old modules, fix any
-- qualified symbol references, prettyprint and return.
moduleDecls :: Set S.ModuleName -> S.ModuleName -> (S.ModuleName, Module, String) -> String
moduleDecls old new (S.ModuleName _, m, text) =
    foldModule (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ _ _ _ r -> r)
               (\ d pre s r ->
                    let d' = sDecl d
                        d'' = fixReferences old new d' in
                    r <>
                    -- Omit the first pre of each module, it probably contains ") where"
                    (if r /= "" then pre else "") <>
                    (if d'' /= d' then prettyPrintWithMode defaultMode d'' <> "\n\n" else s))
               (\ s r -> r <> s)
               m text "" <> "\n"

-- | Change any ModuleName in 'old' to 'new'.  Note that this will
-- probably mess up the location information, so the result (if
-- different from the original) should be prettyprinted, not
-- exactPrinted.
fixReferences :: (Data a, Typeable a) => Set S.ModuleName -> S.ModuleName -> a -> a
fixReferences old new x =
    everywhere (mkT moveModuleName) x
    where
      moveModuleName :: S.ModuleName -> S.ModuleName
      moveModuleName name@(S.ModuleName _) = if member name old then new else name

test1 :: Test
test1 =
    TestCase
      (system "rsync -aHxS --delete testdata/original/ testdata/copy" >>
       withCurrentDirectory "testdata/copy"
         (runCleanT "dist/scratch"
          (catModules
           (Set.fromList testModules)
           [S.ModuleName "Debian.Repo.AptCache", S.ModuleName "Debian.Repo.AptImage"]
           (S.ModuleName "Debian.Repo.Cache")) >>
            assertEqual
              "catModules"
              ()
              ()))

test2 :: Test
test2 =
    TestCase
      (system "rsync -aHxS --delete testdata/original/ testdata/copy" >>
       withCurrentDirectory "testdata/copy"
         (runCleanT "dist/scratch"
          (catModules
           (Set.fromList testModules)
           [S.ModuleName "Debian.Repo.Types.Slice", S.ModuleName "Debian.Repo.Types.Repo", S.ModuleName "Debian.Repo.Types.EnvPath"]
           (S.ModuleName "Debian.Repo.Types.Common")) >>
          mapM_ removeFileIfPresent junk) >>
       readProcessWithExitCode "diff" ["-ru", "--unidirectional-new-file", "testdata/original", "testdata/copy"] "" >>= \ (code, out, err) ->
       let out' = unlines (List.filter (not . isPrefixOf "Binary files") . List.map (takeWhile (/= '\t')) $ (lines out)) in
       assertEqual "catModules" (ExitFailure 2, expected, "") (code, out', err))

expected :: String
expected =
    unlines
    [ "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/AptCache.hs testdata/copy/Debian/Repo/AptCache.hs",
      "--- testdata/original/Debian/Repo/AptCache.hs",
      "+++ testdata/copy/Debian/Repo/AptCache.hs",
      "@@ -46,12 +46,12 @@",
      " import Debian.Repo.SourcesList (parseSourcesList)",
      " import Debian.Repo.Types.AptBuildCache (AptBuildCache(aptSliceList))",
      " import Debian.Repo.Types.AptCache (AptCache(..), AptCache(aptArch, aptBaseSliceList, aptBinaryPackages, aptReleaseName, aptSourcePackages, globalCacheDir))",
      "-import Debian.Repo.Types.EnvPath (EnvRoot(..), EnvRoot(EnvRoot))",
      "+import Debian.Repo.Types.Common (EnvRoot(..), EnvRoot(EnvRoot))",
      " import Debian.Repo.Types.PackageIndex (BinaryPackage(packageID), binaryPackageName, PackageID(packageVersion), PackageIndex(..), SourcePackage(sourcePackageID), sourcePackageName)",
      " import Debian.Repo.Types.Release (Release(releaseName))",
      "-import Debian.Repo.Types.Repo (Repo(repoReleaseInfo), repoKey, RepoKey)",
      "+import Debian.Repo.Types.Common (Repo(repoReleaseInfo), repoKey, RepoKey)",
      " import Debian.Repo.Types.Repository (MonadRepoCache, prepareRepository)",
      "-import Debian.Repo.Types.Slice (Slice(..), SliceList(slices))",
      "+import Debian.Repo.Types.Common (Slice(..), SliceList(slices))",
      " import Debian.Sources (DebSource(..), SourceType(..))",
      " import Debian.Version (DebianVersion, prettyDebianVersion)",
      " import Extra.Files (replaceFile)",
      "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/AptImage.hs testdata/copy/Debian/Repo/AptImage.hs",
      "--- testdata/original/Debian/Repo/AptImage.hs",
      "+++ testdata/copy/Debian/Repo/AptImage.hs",
      "@@ -16,9 +16,9 @@",
      " import Debian.Repo.AptCache (aptOpts, buildArchOfRoot, cacheRootDir, getBinaryPackagesBase, getSourcePackagesBase, SourcesChangedAction, updateCacheSources)",
      " import Debian.Repo.Monads.Apt (insertAptImage, lookupAptImage, MonadApt(getApt, putApt))",
      " import Debian.Repo.Types.AptImage (AptImage(..))",
      "-import Debian.Repo.Types.EnvPath (EnvRoot(..))",
      "+import Debian.Repo.Types.Common (EnvRoot(..))",
      " import Debian.Repo.Types.PackageIndex (PackageID(packageVersion), SourcePackage(sourcePackageID))",
      "-import Debian.Repo.Types.Slice (NamedSliceList(sliceList, sliceListName))",
      "+import Debian.Repo.Types.Common (NamedSliceList(sliceList, sliceListName))",
      " import Debian.Sources (SliceName(sliceName))",
      " import Extra.Files (replaceFile, writeFileIfMissing)",
      " import System.Directory (createDirectoryIfMissing)",
      "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/Monads/Apt.hs testdata/copy/Debian/Repo/Monads/Apt.hs",
      "--- testdata/original/Debian/Repo/Monads/Apt.hs",
      "+++ testdata/copy/Debian/Repo/Monads/Apt.hs",
      "@@ -41,7 +41,7 @@",
      " import qualified Debian.Control.Text as B (Control'(Control), ControlFunctions(parseControlFromHandle), Paragraph)",
      " import Debian.Release (ReleaseName)",
      " import Debian.Repo.Types (AptImage, BinaryPackage, Release, SourcePackage)",
      "-import Debian.Repo.Types.Repo (Repo(repoKey), RepoKey(..))",
      "+import Debian.Repo.Types.Common (Repo(repoKey), RepoKey(..))",
      " import Debian.Repo.Types.Repository (MonadRepoCache(getRepoCache, putRepoCache), Repository)",
      " import Debian.Sources (SliceName)",
      " import qualified System.IO as IO (hClose, IOMode(ReadMode), openBinaryFile)",
      "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/Package.hs testdata/copy/Debian/Repo/Package.hs",
      "--- testdata/original/Debian/Repo/Package.hs",
      "+++ testdata/copy/Debian/Repo/Package.hs",
      "@@ -41,10 +41,10 @@",
      " import Debian.Repo.Monads.Apt (insertBinaryPackages, insertSourcePackages, lookupBinaryPackages, lookupSourcePackages, MonadApt(getApt, putApt), readParagraphs)",
      " import Debian.Repo.PackageIndex (binaryIndexList, packageIndexPath, sourceIndexList)",
      " import Debian.Repo.Types.AptCache (AptCache(aptArch, rootDir))",
      "-import Debian.Repo.Types.EnvPath (EnvRoot(rootPath), outsidePath)",
      "+import Debian.Repo.Types.Common (EnvRoot(rootPath), outsidePath)",
      " import Debian.Repo.Types.PackageIndex (BinaryPackage(..), BinaryPackageLocal, binaryPackageName, makeBinaryPackageID, makeSourcePackageID, PackageID(..), PackageIndex(..), PackageIndexLocal, SourceControl(..), SourceFileSpec(SourceFileSpec, sourceFileName), SourcePackage(..))",
      " import Debian.Repo.Types.Release (Release(releaseName))",
      "-import Debian.Repo.Types.Repo (RepoKey, repoKeyURI)",
      "+import Debian.Repo.Types.Common (RepoKey, repoKeyURI)",
      " import Debian.Repo.Types.Repository (LocalRepository, MonadRepoCache, repoRoot)",
      " import Debian.URI (fileFromURIStrict)",
      " import Debian.Version (DebianVersion, parseDebianVersion)",
      "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/PackageIndex.hs testdata/copy/Debian/Repo/PackageIndex.hs",
      "--- testdata/original/Debian/Repo/PackageIndex.hs",
      "+++ testdata/copy/Debian/Repo/PackageIndex.hs",
      "@@ -15,7 +15,7 @@",
      " import Debian.Arch (Arch(..), prettyArch)",
      " import Debian.Release (releaseName', sectionName')",
      " import Debian.Repo.Types (PackageIndex(..), Release(..))",
      "-import Debian.Repo.Types.Repo (repoURI)",
      "+import Debian.Repo.Types.Common (repoURI)",
      " import Debian.Repo.Types.Repository (Repository)",
      " import Debian.Sources (DebSource(..), SourceType(..))",
      " import System.FilePath ((</>))",
      "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/Slice.hs testdata/copy/Debian/Repo/Slice.hs",
      "--- testdata/original/Debian/Repo/Slice.hs",
      "+++ testdata/copy/Debian/Repo/Slice.hs",
      "@@ -29,9 +29,9 @@",
      " import Debian.Repo.Monads.Apt (MonadApt)",
      " import Debian.Repo.SourcesList (parseSourceLine, parseSourcesList)",
      " import Debian.Repo.Types (EnvPath(..), EnvRoot(..))",
      "-import Debian.Repo.Types.Repo (repoKey, RepoKey(..))",
      "+import Debian.Repo.Types.Common (repoKey, RepoKey(..))",
      " import Debian.Repo.Types.Repository (prepareRepository)",
      "-import Debian.Repo.Types.Slice (NamedSliceList(..), Slice(..), SliceList(..))",
      "+import Debian.Repo.Types.Common (NamedSliceList(..), Slice(..), SliceList(..))",
      " import Debian.Sources (DebSource(..), SliceName(SliceName), SourceType(..))",
      " import Debian.URI (dirFromURI, fileFromURI, toURI')",
      " import Network.URI (URI(uriScheme, uriPath))",
      "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/Types/AptBuildCache.hs testdata/copy/Debian/Repo/Types/AptBuildCache.hs",
      "--- testdata/original/Debian/Repo/Types/AptBuildCache.hs",
      "+++ testdata/copy/Debian/Repo/Types/AptBuildCache.hs",
      "@@ -4,12 +4,8 @@",
      "     ( AptBuildCache(..)",
      "     ) where",
      " ",
      "-import Debian.Arch (Arch(..))",
      "-import Debian.Release (ReleaseName(..))",
      " import Debian.Repo.Types.AptCache (AptCache)",
      "-import Debian.Repo.Types.EnvPath (EnvRoot)",
      "-import Debian.Repo.Types.PackageIndex (BinaryPackage, SourcePackage)",
      "-import Debian.Repo.Types.Slice (SliceList)",
      "+import Debian.Repo.Types.Common (SliceList)",
      " ",
      " -- | An apt cache with extra sources.list lines for a local upload repository.",
      " class AptCache t => AptBuildCache t where",
      "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/Types/AptCache.hs testdata/copy/Debian/Repo/Types/AptCache.hs",
      "--- testdata/original/Debian/Repo/Types/AptCache.hs",
      "+++ testdata/copy/Debian/Repo/Types/AptCache.hs",
      "@@ -6,9 +6,9 @@",
      " ",
      " import Debian.Arch (Arch(..))",
      " import Debian.Release (ReleaseName(..))",
      "-import Debian.Repo.Types.EnvPath (EnvRoot)",
      "+import Debian.Repo.Types.Common (EnvRoot)",
      " import Debian.Repo.Types.PackageIndex (BinaryPackage, SourcePackage)",
      "-import Debian.Repo.Types.Slice (SliceList)",
      "+import Debian.Repo.Types.Common (SliceList)",
      " ",
      " {-",
      " instance Show FileStatus where",
      "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/Types/AptImage.hs testdata/copy/Debian/Repo/Types/AptImage.hs",
      "--- testdata/original/Debian/Repo/Types/AptImage.hs",
      "+++ testdata/copy/Debian/Repo/Types/AptImage.hs",
      "@@ -7,9 +7,9 @@",
      " import Debian.Arch (Arch(..))",
      " import Debian.Release (ReleaseName(..))",
      " import Debian.Repo.Types.AptCache (AptCache(..))",
      "-import Debian.Repo.Types.EnvPath (EnvRoot)",
      "+import Debian.Repo.Types.Common (EnvRoot)",
      " import Debian.Repo.Types.PackageIndex (BinaryPackage, SourcePackage)",
      "-import Debian.Repo.Types.Slice (SliceList)",
      "+import Debian.Repo.Types.Common (SliceList)",
      " ",
      " {-",
      " instance Show FileStatus where",
      "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/Types/Common.hs testdata/copy/Debian/Repo/Types/Common.hs",
      "--- testdata/original/Debian/Repo/Types/Common.hs",
      "+++ testdata/copy/Debian/Repo/Types/Common.hs",
      "@@ -0,0 +1,108 @@",
      "+{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}",
      "+{-# OPTIONS_GHC -fno-warn-orphans #-}",
      "+module Debian.Repo.Types.Common",
      "+    ( NamedSliceList(..), SliceList(..), Slice(..), compatibilityFile, libraryCompatibilityLevel, repoKeyURI, repoURI, RepoKey(..), Repo(..), rootEnvPath, appendPath, outsidePath, EnvPath(..), EnvRoot(..)",
      "+    ) where",
      "+",
      "+import Debian.Sources (DebSource(..), SliceName(..), SourceType(..))",
      "+import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), vcat)",
      "+import Control.Exception (throw)",
      "+import Data.Char (isDigit)",
      "+import Data.Maybe (fromJust)",
      "+import Data.Text (unpack)",
      "+import Debian.Repo.Types.Release (Release)",
      "+import Debian.URI (fileFromURI, fromURI', URI')",
      "+import qualified Debian.UTF8 as Deb (decode)",
      "+import Network.URI (parseURI, URI(uriPath))",
      "+import System.FilePath ((</>))",
      "+",
      "+",
      "+",
      "+data Slice = Slice {sliceRepoKey :: RepoKey, sliceSource :: DebSource} deriving (Eq, Ord, Show)",
      "+",
      "+-- | Each line of the sources.list represents a slice of a repository",
      "+data SliceList = SliceList {slices :: [Slice]} deriving (Eq, Ord, Show)",
      "+",
      "+data NamedSliceList",
      "+    = NamedSliceList { sliceList :: SliceList",
      "+                     , sliceListName :: SliceName",
      "+                     } deriving (Eq, Ord, Show)",
      "+",
      "+instance Pretty SliceList where",
      "+    pretty = vcat . map (pretty . sliceSource) . slices",
      "+",
      "+deriving instance Show SourceType",
      "+deriving instance Show DebSource",
      "+",
      "+",
      "+data RepoKey",
      "+    = Remote URI'",
      "+    | Local EnvPath",
      "+      deriving (Read, Show, Eq, Ord)",
      "+",
      "+class (Ord t, Eq t) => Repo t where",
      "+    repoKey :: t -> RepoKey",
      "+    repositoryCompatibilityLevel :: t -> IO (Maybe Int)",
      "+    repositoryCompatibilityLevel r =",
      "+        fileFromURI uri' >>= either throw (return . parse . unpack . Deb.decode)",
      "+        where",
      "+          uri' = uri {uriPath = uriPath uri </> compatibilityFile}",
      "+          uri = case repoKey r of",
      "+                  Remote x -> fromURI' x",
      "+                  Local x -> fromJust . parseURI $ \"file://\" ++ envPath x",
      "+          parse :: String -> Maybe Int",
      "+          parse s = case takeWhile isDigit s of",
      "+                         \"\" -> Nothing",
      "+                         s' -> Just . read $ s'",
      "+    -- | This method returns a list of all the release in the",
      "+    -- repository.  This can be used to identify all of the files",
      "+    -- in the repository that are not garbage.",
      "+    repoReleaseInfo :: t -> [Release]",
      "+    checkCompatibility :: t -> IO ()",
      "+    checkCompatibility repo =",
      "+        do level <- repositoryCompatibilityLevel repo",
      "+           case level of",
      "+             Nothing -> return ()",
      "+             Just n | n >= libraryCompatibilityLevel -> return ()",
      "+             Just n -> error (\"Compatibility error: repository level \" ++ show n ++",
      "+                              \" < library level \" ++ show libraryCompatibilityLevel ++ \", please upgrade.\")",
      "+",
      "+-- |The name of the file which holds the repository's compatibility",
      "+-- level.",
      "+compatibilityFile :: FilePath",
      "+compatibilityFile = \"repository-compat\"",
      "+",
      "+-- | The compatibility level of this library and any applications",
      "+-- which use it.  It is an error if we try to use a repository whose",
      "+-- compatibility level is higher than this, a newer version of the",
      "+-- library must be used.  This value was increased from 1 to 2 due",
      "+-- to a new version number tagging policy.",
      "+libraryCompatibilityLevel :: Int",
      "+libraryCompatibilityLevel = 2",
      "+",
      "+repoURI :: Repo r => r -> URI",
      "+repoURI = repoKeyURI . repoKey",
      "+",
      "+repoKeyURI :: RepoKey -> URI",
      "+repoKeyURI (Local path) = fromJust . parseURI $ \"file://\" ++ envPath path",
      "+repoKeyURI (Remote uri) = fromURI' uri",
      "+",
      "+",
      "+data EnvRoot = EnvRoot { rootPath :: FilePath } deriving (Ord, Eq, Read, Show)",
      "+",
      "+-- |A directory inside of an OS image.",
      "+data EnvPath = EnvPath { envRoot :: EnvRoot",
      "+                       , envPath :: FilePath",
      "+                       } deriving (Ord, Eq, Read, Show)",
      "+",
      "+outsidePath :: EnvPath -> FilePath",
      "+outsidePath path = rootPath (envRoot path) ++ envPath path",
      "+",
      "+appendPath :: FilePath -> EnvPath -> EnvPath",
      "+appendPath suff path = path { envPath = envPath path ++ suff }",
      "+",
      "+rootEnvPath :: FilePath -> EnvPath",
      "+rootEnvPath s = EnvPath { envRoot = EnvRoot \"\", envPath = s }",
      "+",
      "+",
      "+",
      "Only in testdata/original/Debian/Repo/Types: EnvPath.hs",
      "Only in testdata/original/Debian/Repo/Types: Repo.hs",
      "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/Types/Repository.hs testdata/copy/Debian/Repo/Types/Repository.hs",
      "--- testdata/original/Debian/Repo/Types/Repository.hs",
      "+++ testdata/copy/Debian/Repo/Types/Repository.hs",
      "@@ -34,9 +34,9 @@",
      " import Debian.Release (parseReleaseName, ReleaseName(..), releaseName', Section, sectionName', SubSection(section))",
      " import Debian.Repo.Monads.Top (MonadTop, sub)",
      " import Debian.Repo.Sync (rsync)",
      "-import Debian.Repo.Types.EnvPath (EnvPath(EnvPath), EnvRoot(EnvRoot), outsidePath)",
      "+import Debian.Repo.Types.Common (EnvPath(EnvPath), EnvRoot(EnvRoot), outsidePath)",
      " import Debian.Repo.Types.Release (makeReleaseInfo, Release(releaseName))",
      "-import Debian.Repo.Types.Repo (compatibilityFile, libraryCompatibilityLevel, Repo(..), RepoKey(..))",
      "+import Debian.Repo.Types.Common (compatibilityFile, libraryCompatibilityLevel, Repo(..), RepoKey(..))",
      " import Debian.URI (dirFromURI, fileFromURI, fromURI', toURI', URI(uriScheme, uriPath), URI', uriToString')",
      " import Debian.UTF8 as Deb (decode)",
      " import Extra.Files (maybeWriteFile)",
      "Only in testdata/original/Debian/Repo/Types: Slice.hs",
      "diff -ru --unidirectional-new-file testdata/original/Debian/Repo/Types.hs testdata/copy/Debian/Repo/Types.hs",
      "--- testdata/original/Debian/Repo/Types.hs",
      "+++ testdata/copy/Debian/Repo/Types.hs",
      "@@ -1,11 +1,11 @@",
      " module Debian.Repo.Types",
      "     ( module Debian.Repo.Types.AptImage",
      "-    , module Debian.Repo.Types.EnvPath",
      "+     , module Debian.Repo.Types.Common",
      "     , module Debian.Repo.Types.PackageIndex",
      "     , module Debian.Repo.Types.Release",
      "     ) where",
      " ",
      "-import Debian.Repo.Types.AptImage",
      "-import Debian.Repo.Types.EnvPath",
      "-import Debian.Repo.Types.PackageIndex",
      "-import Debian.Repo.Types.Release",
      "+import Debian.Repo.Types.AptImage (AptImage(..))",
      "+import Debian.Repo.Types.Common (appendPath, compatibilityFile, EnvPath(..), EnvRoot(..), libraryCompatibilityLevel, NamedSliceList(..), outsidePath, Repo(..), RepoKey(..), repoKeyURI, repoURI, rootEnvPath, Slice(..), SliceList(..))",
      "+import Debian.Repo.Types.PackageIndex (BinaryPackage(..), BinaryPackageLocal, binaryPackageName, makeBinaryPackageID, makeSourcePackageID, PackageID(..), PackageIDLocal, PackageIndex(..), PackageIndexLocal, PackageVersion(..), PkgVersion(..), prettyBinaryPackage, prettyPackageID, prettyPkgVersion, SourceControl(..), SourceFileSpec(..), SourcePackage(..), SourcePackageLocal, sourcePackageName)",
      "+import Debian.Repo.Types.Release (makeReleaseInfo, parseArchitectures, parseComponents, Release(..))"]

junk :: [String]
junk =
    [ "Debian.Repo.Monads.Top.imports"
    , "Debian.Repo.Orphans.imports"
    , "Debian.Repo.SourcesList.imports"
    , "Debian.Repo.Sync.imports"
    , "Debian.Repo.Types.PackageIndex.imports"
    , "Debian.Repo.Types.Release.imports"
    , "Text.Format.imports"
    , "Tmp.File.imports"
    , "Debian/Repo/Package.hs~"
    , "Debian/Repo/PackageIndex.hs~"
    , "Debian/Repo/Types/AptBuildCache.hs~"
    , "Debian/Repo/Types/EnvPath.hs~"
    , "Debian/Repo/Types/Repo.hs~"
    , "Debian/Repo/Types/AptImage.hs~"
    , "Debian/Repo/Types/AptCache.hs~"
    , "Debian/Repo/Types/Repository.hs~"
    , "Debian/Repo/Types/Common.hs~"
    , "Debian/Repo/AptImage.hs~"
    , "Debian/Repo/Slice.hs~"
    , "Debian/Repo/AptCache.hs~"
    , "Debian/Repo/Types.hs~"
    , "Debian/Repo/Monads/Apt.hs~" ]

testModules :: [S.ModuleName]
testModules =
            [S.ModuleName "Debian.Repo.Sync",
             S.ModuleName "Debian.Repo.Slice",
             S.ModuleName "Debian.Repo.SourcesList",
             S.ModuleName "Debian.Repo.PackageIndex",
             S.ModuleName "Debian.Repo.Types",
             S.ModuleName "Debian.Repo.Types.Slice",
             S.ModuleName "Debian.Repo.Types.Repository",
             S.ModuleName "Debian.Repo.Types.PackageIndex",
             S.ModuleName "Debian.Repo.Types.Release",
             S.ModuleName "Debian.Repo.Types.AptImage",
             S.ModuleName "Debian.Repo.Types.Repo",
             S.ModuleName "Debian.Repo.Types.AptBuildCache",
             S.ModuleName "Debian.Repo.Types.EnvPath",
             S.ModuleName "Debian.Repo.Types.AptCache",
             S.ModuleName "Debian.Repo.Orphans",
             S.ModuleName "Debian.Repo.Types",
             S.ModuleName "Debian.Repo.AptImage",
             S.ModuleName "Debian.Repo.Package",
             S.ModuleName "Debian.Repo.Monads.Top",
             S.ModuleName "Debian.Repo.Monads.Apt",
             S.ModuleName "Debian.Repo.AptCache",
             S.ModuleName "Tmp.File",
             S.ModuleName "Text.Format"]
