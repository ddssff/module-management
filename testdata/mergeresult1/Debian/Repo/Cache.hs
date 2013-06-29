{- This is a comment at the top -}   
{-# LANGUAGE DeriveDataTypeable, PackageImports #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |An AptCache represents a local cache of a remote repository.  The
-- cached information is usually downloaded by running "apt-get
-- update", and appears in @\/var\/lib\/apt\/lists@.
module Debian.Repo.Cache {-# WARNING "this is a warning" #-}
    ( SourcesChangedAction(..), aptSourcePackagesSorted, sliceIndexes, cacheDistDir, distDir, aptDir, cacheRootDir, cacheSourcesPath, sourcesPath, sourceDir, aptCacheFiles, aptCacheFilesOfSlice, archFiles, buildArchOfEnv, buildArchOfRoot, updateCacheSources, sourcePackages, binaryPackages, runAptGet, aptOpts, getSourcePackagesBase, getBinaryPackagesBase, getSourcePackagesBuild, getBinaryPackagesBuild, prepareAptEnv, updateAptEnv
    ) where

import Control.DeepSeq (force, NFData)
import "mtl" Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString.Lazy as L (empty)
import Data.Data (Data)
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Typeable (Typeable)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..), prettyArch)
import Debian.Relation (BinPkgName, PkgName, SrcPkgName(..))
import Debian.Release (ReleaseName(..), ReleaseName(relName), releaseName', sectionName')
import Debian.Repo.Monads.Apt (insertAptImage, lookupAptImage, MonadApt, MonadApt(getApt, putApt))
import Debian.Repo.Package (binaryPackagesOfCachedIndex, sourcePackagesOfCachedIndex)
import Debian.Repo.Slice (binarySlices, sourceSlices, verifySourcesList)
import Debian.Repo.SourcesList (parseSourcesList)
import Debian.Repo.Types.AptBuildCache (AptBuildCache(aptSliceList))
import Debian.Repo.Types.AptCache (AptCache(..), AptCache(aptArch, aptBaseSliceList, aptBinaryPackages, aptReleaseName, aptSourcePackages, globalCacheDir))
import Debian.Repo.Types.AptImage (AptImage(..))
import Debian.Repo.Types.EnvPath (EnvRoot(..), EnvRoot(EnvRoot))
import Debian.Repo.Types.PackageIndex (BinaryPackage(packageID), binaryPackageName, PackageID(packageVersion), PackageIndex(..), SourcePackage(sourcePackageID), sourcePackageName)
import Debian.Repo.Types.Release (Release(releaseName))
import Debian.Repo.Types.Repo (Repo(repoReleaseInfo), repoKey, RepoKey)
import Debian.Repo.Types.Repository (MonadRepoCache, prepareRepository)
import Debian.Repo.Types.Slice (NamedSliceList(sliceList, sliceListName), Slice(..), SliceList(slices))
import Debian.Sources (DebSource(..), SliceName(sliceName), SourceType(..))
import Debian.Version (DebianVersion, prettyDebianVersion)
import Extra.Files (replaceFile, writeFileIfMissing)
import Network.URI (escapeURIString, URI(uriAuthority, uriPath, uriScheme), URIAuth(uriPort, uriRegName, uriUserInfo))
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO (hGetLine, stdin)
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode, shell)
import System.Process.Progress (ePutStr, ePutStrLn, qPutStrLn, quieter, runProcessF)
import System.Unix.Chroot (useEnv)
import System.Unix.Directory (removeRecursiveSafely)
import Text.PrettyPrint.ANSI.Leijen (pretty)

instance NFData ExitCode

-- The following are path functions which can be used while
-- constructing instances of AptCache.  Each is followed by a
-- corresponding function that gives the same result when applied to
-- an AptCache instance.

-- | A directory which will hold all the cached files for this
-- NamedSliceList.
cacheDistDir :: FilePath -> ReleaseName -> FilePath
cacheDistDir cacheDir release = cacheDir ++ "/dists/" ++ relName release

cacheRootDir :: FilePath -> ReleaseName -> EnvRoot
cacheRootDir cacheDir release = EnvRoot (cacheDistDir cacheDir release ++ "/aptEnv")

distDir :: AptCache c => c -> FilePath
distDir cache = cacheDistDir (globalCacheDir cache) (aptReleaseName cache)

aptDir :: AptCache c => c -> SrcPkgName -> FilePath
aptDir cache package = distDir cache ++ "/apt/" ++ unSrcPkgName package

-- | The path where a text of the SliceList is stored.
cacheSourcesPath :: FilePath -> ReleaseName -> FilePath
cacheSourcesPath cacheDir release = cacheDistDir cacheDir release </> "sources"

sourcesPath :: AptCache c => c -> FilePath
sourcesPath cache = cacheSourcesPath (globalCacheDir cache) (aptReleaseName cache)

-- Additional functions which can only be used on already constructed
-- instances of AptCache.

-- | A directory holding all files downloaded by apt-get source for a
-- certain package
sourceDir :: AptCache t => t -> String -> FilePath
sourceDir c package = distDir c ++ "/apt/" ++ package

-- |Return all the named source packages sorted by version
aptSourcePackagesSorted :: AptCache t => t -> [SrcPkgName] -> [SourcePackage]
aptSourcePackagesSorted os names =
    sortBy cmp . filterNames names . aptSourcePackages $ os
    where
      filterNames names' packages =
          filter (flip elem names' . sourcePackageName) packages
      cmp p1 p2 =
          compare v2 v1         -- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

-- |Return a list of the index files that contain the packages of a
-- slice.
sliceIndexes :: (MonadRepoCache m, AptCache a) => a -> Slice -> m [(RepoKey, Release, PackageIndex)]
sliceIndexes cache slice =
    prepareRepository (sliceRepoKey slice) >>= \ repo ->
    case (sourceDist (sliceSource slice)) of
      Left exact -> error $ "Can't handle exact path in sources.list: " ++ exact
      Right (release, sections) -> return $ map (makeIndex repo release) sections
    where
      makeIndex repo release section =
          (repoKey repo,
           findReleaseInfo repo release,
           PackageIndex { packageIndexComponent = section
                        , packageIndexArch = case (sourceType (sliceSource slice)) of
                                               DebSrc -> Source
                                               Deb -> aptArch cache })
      findReleaseInfo repo release =
          case filter ((==) release . releaseName) (repoReleaseInfo repo) of
            [x] -> x
            [] -> error $ ("sliceIndexes: Invalid release name: " ++ releaseName' release ++
                           "\n  You may need to remove ~/.autobuilder/repoCache." ++
                           "\n  Available: " ++ (show . map releaseName . repoReleaseInfo $ repo)) ++
                           "\n repoKey: " ++ show (repoKey repo) ++
                           "\n repoReleaseInfo: " ++ show (repoReleaseInfo repo) ++
                           "\n slice: " ++ show slice
            xs -> error $ "Internal error 5 - multiple releases named " ++ releaseName' release ++ "\n" ++ show xs

-- |Return the paths in the local cache of the index files of a slice list.
aptCacheFiles :: AptCache a => a -> [DebSource] -> [FilePath]
aptCacheFiles apt sources = concat . map (aptCacheFilesOfSlice apt) $ sources

-- |Return the paths in the local cache of the index files of a single slice.
aptCacheFilesOfSlice :: AptCache a => a -> DebSource -> [FilePath]
aptCacheFilesOfSlice apt slice = archFiles (aptArch apt) slice

-- |Return the list of files that apt-get update would write into
-- \/var\/lib\/apt\/lists when it processed the given list of DebSource.
archFiles :: Arch -> DebSource -> [FilePath]
archFiles arch deb =
    case (arch, deb) of
      (Binary _ _, DebSource DebSrc _ _) ->
          map (++ "_source_Sources") (archFiles' deb)
      (Binary _os _cpu, DebSource Deb _ _) ->
          map (++ ("_binary-" ++ show (prettyArch arch) ++ "_Packages")) (archFiles' deb)
      (x, _) -> error $ "Invalid build architecture: " ++ show x

archFiles' :: DebSource -> [FilePath]
archFiles' deb =
    let uri = sourceUri deb
        distro = sourceDist deb in
    let scheme = uriScheme uri
        auth = uriAuthority uri
        path = uriPath uri in
    let userpass = maybe "" uriUserInfo auth
        reg = maybeOfString $ maybe "" uriRegName auth
        port = maybe "" uriPort auth in
    let (user, pass) = break (== ':') userpass in
    let user' = maybeOfString user
        pass' = maybeOfString pass in
    let uriText = prefix scheme user' pass' reg port path in
    -- what about dist?
    either (\ exact -> [(escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText ++ escape exact))])
           (\ (dist, sections) ->
                map (\ section ->
                         (escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText +?+ "dists_") ++
                          releaseName' dist ++ "_" ++ sectionName' section))
                    sections)
           distro
    where
      -- If user is given and password is not, the user name is
      -- added to the file name.  Otherwise it is not.  Really.
      prefix "http:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "http:" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix "ftp:" _ _ (Just host) _ path =
          host ++ escape path
      prefix "file:" Nothing Nothing Nothing "" path =
          escape path
      prefix "ssh:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "ssh" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix _ _ _ _ _ _ = error ("invalid DebSource: " ++ show (pretty deb))
      maybeOfString "" = Nothing
      maybeOfString s = Just s
      escape s = intercalate "_" (wordsBy (== '/') s)

buildArchOfEnv :: EnvRoot -> IO Arch
buildArchOfEnv (EnvRoot root)  =
    do setEnv "LOGNAME" "root" True -- This is required for dpkg-architecture to work in a build environment
       a@(code1, out1, _err1) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
       b@(code2, out2, _err2) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
       case (code1, lines out1, code2, lines out2) of
         (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
             return $ Binary (ArchOS os) (ArchCPU cpu)
         _ -> error $ "Failure computing build architecture of build env at " ++ root ++ ": " ++ show (a, b)
{-
  (err, _) <- useEnv root forceList (readProcessChunks (shell cmd) L.empty) >>= return . collectOutputs
       case code of
         (ExitSuccess : _) ->
             case words (UTF8.toString (B.concat (L.toChunks out))) of
               [] -> error $ "Invalid output from " ++ cmd
               (arch : _) -> return (Binary arch)
         _ -> error $ "Failure: " ++ cmd ++ " -> " ++ show code ++ "\n\nstdout:\n\n" ++ show out ++ "\n\nstderr:\n\n" ++ show err
    where
      cmd = "export LOGNAME=root; dpkg-architecture -qDEB_BUILD_ARCH"
-}

buildArchOfRoot :: IO Arch
buildArchOfRoot =
    do a@(code1, out1, _err1) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
       b@(code2, out2, _err2) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
       case (code1, lines out1, code2, lines out2) of
         (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
             return $ Binary (parseArchOS os) (parseArchCPU cpu)
         _ -> error $ "Failure computing build architecture of /: " ++ show (a, b)
    where
      parseArchOS "any" = ArchOSAny
      parseArchOS x = ArchOS x
      parseArchCPU "any" = ArchCPUAny
      parseArchCPU x = ArchCPU x
{-
    do (code, out, err, _) <- runProcess (shell cmd) L.empty >>= return . collectOutputs
       case code of
         (ExitSuccess : _) ->
             case words (UTF8.toString (B.concat (L.toChunks out))) of
               [] -> error $ "Invalid output from " ++ cmd
               (arch : _) -> return (Binary arch)
         _ -> error $ "Failure: " ++ cmd ++ " -> " ++ show code ++ "\n\nstdout:\n\n" ++ show out ++ "\n\nstderr:\n\n" ++ show err
    where
      cmd = "dpkg-architecture -qDEB_BUILD_ARCH"
-}

(+?+) :: String -> String -> String
(+?+) a ('_' : b) = a +?+ b
(+?+) "" b = b
(+?+) a b =
    case last a of
      '_' -> (init a) +?+ b
      _ -> a ++ "_" ++ b

wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
wordsBy p s =
    case (break p s) of
      (s', []) -> [s']
      (h, t) -> h : wordsBy p (drop 1 t)

data SourcesChangedAction =
    SourcesChangedError |
    UpdateSources |
    RemoveRelease
    deriving (Eq, Show, Data, Typeable)

-- |Change the sources.list of an AptCache object, subject to the
-- value of sourcesChangedAction.
updateCacheSources :: (MonadApt m, AptCache c) => SourcesChangedAction -> c -> m c
updateCacheSources sourcesChangedAction distro
  = qPutStrLn "Updating cache sources" >>
      do let baseSources = aptBaseSliceList distro
         let dir = Debian.Repo.Cache.distDir distro
         distExists <- liftIO $
                         doesFileExist (Debian.Repo.Cache.sourcesPath distro)
         case distExists of
             True -> do fileSources <- liftIO
                                         (readFile (Debian.Repo.Cache.sourcesPath distro))
                                         >>= verifySourcesList Nothing . parseSourcesList
                        case (fileSources == baseSources, sourcesChangedAction) of
                            (True, _) -> return ()
                            (False, SourcesChangedError) -> do ePutStrLn
                                                                 ("The sources.list in the existing '"
                                                                    ++
                                                                    relName (aptReleaseName distro)
                                                                      ++
                                                                      "' build environment doesn't match the parameters passed to the autobuilder"
                                                                        ++
                                                                        ":\n\n" ++
                                                                          Debian.Repo.Cache.sourcesPath
                                                                            distro
                                                                            ++
                                                                            ":\n\n" ++
                                                                              show
                                                                                (pretty fileSources)
                                                                                ++
                                                                                "\nRun-time parameters:\n\n"
                                                                                  ++
                                                                                  show
                                                                                    (pretty
                                                                                       baseSources)
                                                                                    ++
                                                                                    "\n" ++
                                                                                      "It is likely that the build environment in\n"
                                                                                        ++
                                                                                        dir ++
                                                                                          " is invalid and should be rebuilt.")
                                                               ePutStr $
                                                                 "Remove it and continue (or exit)?  [y/n]: "
                                                               result <- liftIO $ hGetLine stdin
                                                               case result of
                                                                   ('y' : _) -> do liftIO $
                                                                                     removeRecursiveSafely
                                                                                       dir
                                                                                   liftIO $
                                                                                     createDirectoryIfMissing
                                                                                       True
                                                                                       dir
                                                                                   liftIO $
                                                                                     replaceFile
                                                                                       (Debian.Repo.Cache.sourcesPath
                                                                                          distro)
                                                                                       (show
                                                                                          (pretty
                                                                                             baseSources))
                                                                   _ -> error
                                                                          ("Please remove " ++
                                                                             dir ++ " and restart.")
                            (False, RemoveRelease) -> do ePutStrLn $
                                                           "Removing suspect environment: " ++ dir
                                                         liftIO $ removeRecursiveSafely dir
                                                         liftIO $ createDirectoryIfMissing True dir
                                                         liftIO $
                                                           replaceFile
                                                             (Debian.Repo.Cache.sourcesPath distro)
                                                             (show (pretty baseSources))
                            (False, UpdateSources) -> do ePutStrLn $
                                                           "Updating environment with new sources.list: "
                                                             ++ dir
                                                         liftIO $
                                                           removeFile
                                                             (Debian.Repo.Cache.sourcesPath distro)
                                                         liftIO $
                                                           replaceFile
                                                             (Debian.Repo.Cache.sourcesPath distro)
                                                             (show (pretty baseSources))
             False -> do liftIO $ createDirectoryIfMissing True dir
                         liftIO $
                           replaceFile (Debian.Repo.Cache.sourcesPath distro)
                             (show (pretty baseSources))
         return distro

-- | Return a sorted list of available source packages, newest version first.
sourcePackages :: AptCache a => a -> [SrcPkgName] -> [SourcePackage]
sourcePackages os names =
    sortBy cmp . filterNames . aptSourcePackages $ os
    where
      filterNames :: [SourcePackage] -> [SourcePackage]
      filterNames packages =
          filter (flip elem names . sourcePackageName) packages
      cmp p1 p2 =
          compare v2 v1         -- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

binaryPackages :: AptCache a => a -> [BinPkgName] -> [BinaryPackage]
binaryPackages os names =
    sortBy cmp . filterNames . aptBinaryPackages $ os
    where
      filterNames :: [BinaryPackage] -> [BinaryPackage]
      filterNames packages =
          filter (flip elem names . binaryPackageName) packages
      cmp p1 p2 =
          compare v2 v1         -- Flip args to get newest first
          where
            v1 = packageVersion . packageID $ p1
            v2 = packageVersion . packageID $ p2

getSourcePackagesBase :: (AptCache a, MonadApt m) => a -> m [SourcePackage]
getSourcePackagesBase os =
    do indexes <- mapM (sliceIndexes os) (slices . sourceSlices . aptBaseSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> sourcePackagesOfCachedIndex os repo rel index) indexes >>= return . concat

getSourcePackagesBuild :: (AptBuildCache a, MonadApt m) => a -> m [SourcePackage]
getSourcePackagesBuild os =
    do indexes <- mapM (sliceIndexes os) (slices . sourceSlices . aptSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> sourcePackagesOfCachedIndex os repo rel index) indexes >>= return . concat

getBinaryPackagesBase :: (AptCache a, MonadApt m) => a -> m [BinaryPackage]
getBinaryPackagesBase os =
    do indexes <- mapM (sliceIndexes os) (slices . binarySlices . aptBaseSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> binaryPackagesOfCachedIndex os repo rel index) indexes >>= return . concat

getBinaryPackagesBuild :: (AptBuildCache a, MonadApt m) => a -> m [BinaryPackage]
getBinaryPackagesBuild os =
    do indexes <- mapM (sliceIndexes os) (slices . binarySlices . aptSliceList $ os) >>= return . concat
       mapM (\ (repo, rel, index) -> binaryPackagesOfCachedIndex os repo rel index) indexes >>= return . concat

-- | Note that apt-get source works for binary or source package names.
runAptGet :: (PkgName n, AptCache t) => t -> FilePath -> String -> [(n, Maybe DebianVersion)] -> IO ()
runAptGet os dir command packages =
    createDirectoryIfMissing True dir >> runProcessF (shell cmd) L.empty >> return ()
    where
      cmd = (intercalate " " ("cd" : dir : "&&" : "apt-get" : aptOpts os : command : map formatPackage packages))
      formatPackage (name, Nothing) = show (pretty name)
      formatPackage (name, Just version) = show (pretty name) ++ "=" ++ show (prettyDebianVersion version)

aptOpts :: AptCache t => t -> String
aptOpts os =
    (" -o=Dir::State::status=" ++ root ++ "/var/lib/dpkg/status" ++
     " -o=Dir::State::Lists=" ++ root ++ "/var/lib/apt/lists" ++
     " -o=Dir::Cache::Archives=" ++ root ++ "/var/cache/apt/archives" ++
     " -o=Dir::Etc::SourceList=" ++ root ++ "/etc/apt/sources.list" ++
     " -o=Dir::Etc::SourceParts=" ++ root ++ "/etc/apt/sources.list.d")
    where root = rootPath . rootDir $ os



prepareAptEnv :: MonadApt m =>
                 FilePath               -- Put environment in a subdirectory of this
              -> SourcesChangedAction   -- What to do if environment already exists and sources.list is different
              -> NamedSliceList         -- The sources.list
              -> m AptImage             -- The resulting environment
prepareAptEnv cacheDir sourcesChangedAction sources =
    (\ x -> qPutStrLn ("Preparing apt-get environment for " ++ show (sliceName (sliceListName sources))) >> quieter 2 x) $
    getApt >>= return . lookupAptImage (sliceListName sources) >>=
    maybe (prepareAptEnv' cacheDir sourcesChangedAction sources) return

-- |Create a skeletal enviroment sufficient to run apt-get.
{-# NOINLINE prepareAptEnv' #-}
prepareAptEnv' :: MonadApt m => FilePath -> SourcesChangedAction -> NamedSliceList -> m AptImage
prepareAptEnv' cacheDir sourcesChangedAction sources =
    do let root = rootPath (cacheRootDir cacheDir (ReleaseName (sliceName (sliceListName sources))))
       --vPutStrLn 2 $ "prepareAptEnv " ++ sliceName (sliceListName sources)
       liftIO $ createDirectoryIfMissing True (root ++ "/var/lib/apt/lists/partial")
       liftIO $ createDirectoryIfMissing True (root ++ "/var/lib/apt/lists/partial")
       liftIO $ createDirectoryIfMissing True (root ++ "/var/cache/apt/archives/partial")
       liftIO $ createDirectoryIfMissing True (root ++ "/var/lib/dpkg")
       liftIO $ createDirectoryIfMissing True (root ++ "/etc/apt")
       liftIO $ writeFileIfMissing True (root ++ "/var/lib/dpkg/status") ""
       liftIO $ writeFileIfMissing True (root ++ "/var/lib/dpkg/diversions") ""
       -- We need to create the local pool before updating so the
       -- sources.list will be valid.
       let sourceListText = show (pretty (sliceList sources))
       -- ePut ("writeFile " ++ (root ++ "/etc/apt/sources.list") ++ "\n" ++ sourceListText)
       liftIO $ replaceFile (root ++ "/etc/apt/sources.list") sourceListText
       arch <- liftIO $ buildArchOfRoot
       let os = AptImage { aptGlobalCacheDir = cacheDir
                         , aptImageRoot = EnvRoot root
                         , aptImageArch = arch
                         , aptImageReleaseName = ReleaseName . sliceName . sliceListName $ sources
                         , aptImageSliceList = sliceList sources
                         , aptImageSourcePackages = []
                         , aptImageBinaryPackages = [] }
       os' <- updateCacheSources sourcesChangedAction os >>= updateAptEnv
       getApt >>= putApt . insertAptImage (sliceListName sources) os'
       return os'

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
{-# NOINLINE updateAptEnv #-}
updateAptEnv :: MonadApt m => AptImage -> m AptImage
updateAptEnv os =
    liftIO (runProcessF (shell cmd) L.empty) >>
    getSourcePackagesBase os >>= return . sortBy cmp >>= \ sourcePackages ->
    getBinaryPackagesBase os >>= \ binaryPackages ->
    return $ os { aptImageSourcePackages = sourcePackages
                , aptImageBinaryPackages = binaryPackages }
    where
      cmd = "apt-get" ++ aptOpts os ++ " update"
      -- Flip args to get newest version first
      cmp = flip (compare `on` (packageVersion . sourcePackageID))
{-
      cmp p1 p2 =
          compare v2 v1         -- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

    putStrLn ("> " ++ cmd) >> system cmd >>= \ code ->
    case code of
      ExitSuccess -> return ()
      ExitFailure n -> error $ cmd ++ " -> ExitFailure " ++ show n
-}


