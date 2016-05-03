{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}
module Debian.Repo.State.OSImage
    ( buildArchOfOS
    , osBinaryPackages
    , osSourcePackages
    , prepareOS
    , updateOS
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (force)
import Control.Exception (SomeException, throw)
import Control.Monad (when)
import Control.Monad.Catch (catch, try, MonadMask)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy as L (empty)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import qualified Debian.Debianize.Types.Atoms as EnvSet (EnvSet(cleanOS, dependOS))
import Debian.Pretty (ppDisplay)
import Debian.Relation (BinPkgName(BinPkgName))
import Debian.Release (ReleaseName(relName))
import Debian.Repo.EnvPath (EnvRoot(EnvRoot, rootPath))
import Debian.Repo.Internal.IO (buildArchOfRoot)
import Debian.Repo.Internal.Repos (osFromRoot, MonadRepos, putOSImage)
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.OSImage (createOSImage, OSImage(osArch, osBaseDistro, osLocalMaster,
                                                   osRoot, osSourcePackageCache, osBinaryPackageCache),
                            pbuilder, debootstrap, localeGen, neuterEnv, osFullDistro)
import Debian.Repo.MonadOS (MonadOS(getOS, modifyOS), evalMonadOS, aptGetInstall, syncLocalPool, syncOS)
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Prelude (replaceFile)
import Debian.Repo.Prelude.SSH (sshCopy)
import Debian.Repo.Prelude.Verbosity (ePutStrLn, quieter, qPutStrLn)
import Debian.Repo.Slice (NamedSliceList(sliceListName), Slice(sliceSource), SliceList(slices), SourcesChangedAction(SourcesChangedError), UpdateError(..))
import Debian.Repo.State.PackageIndex (binaryPackagesFromSources, sourcePackagesFromSources)
import Debian.Repo.State.Slice (verifySourcesList)
import Debian.Repo.Top (askTop, distDir, MonadTop, sourcesPath)
import Debian.Sources (DebSource(sourceUri), parseSourcesList)
import Debian.URI (URI(uriScheme))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), splitFileName)
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode, shell)
import System.Process.Chunks (collectProcessTriple)
import Debian.Repo.Prelude.Verbosity (readProcFailing)
import System.Unix.Chroot (useEnv)
import System.Unix.Directory (removeRecursiveSafely)

buildArchOfOS :: (MonadIO m, MonadOS m) => m Arch
buildArchOfOS = do
  root <- rootPath . osRoot <$> getOS
  liftIO $ do
    setEnv "LOGNAME" "root" True -- This is required for dpkg-architecture to work in a build environment
    a@(code1, out1, _err1) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
    b@(code2, out2, _err2) <- useEnv root (return . force) $ readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
    case (code1, lines out1, code2, lines out2) of
      (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
          return $ Binary (ArchOS os) (ArchCPU cpu)
      _ -> error $ "Failure computing build architecture of build env at " ++ root ++ ": " ++ show (a, b)

osSourcePackages :: (MonadRepos m, MonadOS m) => m [SourcePackage]
osSourcePackages = do
  mpkgs <- osSourcePackageCache <$> getOS
  maybe osSourcePackages' return mpkgs
    where
      osSourcePackages' = do
        root <- osRoot <$> getOS
        arch <- osArch <$> getOS
        dist <- osFullDistro <$> getOS
        pkgs <- sourcePackagesFromSources root arch dist
        qPutStrLn ("Read " ++ show (length pkgs) ++ " release source packages")
        modifyOS (\ s -> s {osSourcePackageCache = Just pkgs})
        return pkgs

osBinaryPackages :: (MonadRepos m, MonadOS m) => m [BinaryPackage]
osBinaryPackages = do
  mpkgs <- osBinaryPackageCache <$> getOS
  maybe osBinaryPackages' return mpkgs
    where
      osBinaryPackages' = do
        root <- osRoot <$> getOS
        arch <- osArch <$> getOS
        dist <- osFullDistro <$> getOS
        pkgs <- binaryPackagesFromSources root arch dist
        qPutStrLn ("Read " ++ show (length pkgs) ++ " release binary packages")
        modifyOS (\ s -> s {osBinaryPackageCache = Just pkgs})
        return pkgs

-- |Find or create and update an OS image.  Returns paths to clean and
-- depend os images.
prepareOS
    :: (MonadRepos m, MonadTop m, MonadMask m, MonadIO m) =>
       EnvSet.EnvSet		-- ^ The location where image is to be built
    -> NamedSliceList		-- ^ The sources.list of the base distribution
    -> LocalRepository           -- ^ The location of the local upload repository
    -> Bool			-- ^ If true, remove and rebuild the image
    -> Bool			-- ^ If true, flush all the build dependencies
    -> SourcesChangedAction	-- ^ What to do if called with a sources.list that
				-- differs from the previous call
    -> [String]			-- ^ Extra packages to install - e.g. keyrings
    -> [String]			-- ^ More packages to install, but these may not be available
                                -- immediately - e.g seereason-keyring.  Ignore exceptions.
    -> [String]			-- ^ Packages to exclude
    -> [String]			-- ^ Components of the base repository
    -> m (EnvRoot, EnvRoot)
prepareOS eset distro repo flushRoot flushDepends ifSourcesChanged include optional exclude components =
    do cleanOS <- osFromRoot cleanRoot >>= maybe (do os <- liftIO (createOSImage cleanRoot distro repo)
                                                     putOSImage os
                                                     return os) return
       if flushRoot then evalMonadOS (recreate Flushed) cleanRoot else (qPutStrLn ("Updating " ++ show cleanRoot) >>
                                                                        evalMonadOS updateOS cleanRoot `catch` (\ (e :: UpdateError) -> evalMonadOS (recreate e) cleanRoot))
       evalMonadOS (doInclude >> doLocales) cleanRoot
       when flushDepends (ePutStrLn "sync clean -> depend" >> evalMonadOS (syncOS dependRoot) cleanRoot)
       -- Try running a command in the depend environment, if it fails
       -- sync dependOS from cleanOS.
       dependOS <- osFromRoot dependRoot
       case dependOS of
         Nothing -> do (arch :: Either SomeException Arch) <- (liftIO $ try $ useEnv (rootPath dependRoot) return buildArchOfRoot)
                       case arch of
                         Left _ -> evalMonadOS (syncOS dependRoot) cleanRoot
                         Right _ ->
                             do -- ePutStrLn "createOSImage dependRoot?  I don't understand why this would be done."
                                os <- liftIO (createOSImage dependRoot distro repo)
                                putOSImage os
         Just _ -> return ()
       evalMonadOS syncLocalPool dependRoot
       return (cleanRoot, dependRoot)
    where
      cleanRoot = EnvRoot (EnvSet.cleanOS eset)
      dependRoot = EnvRoot (EnvSet.dependOS eset)
      recreate :: (MonadOS m, MonadTop m, MonadMask m, MonadRepos m, MonadIO m) => UpdateError -> m ()
      recreate (Changed name path computed installed)
          | ifSourcesChanged == SourcesChangedError =
              error $ "FATAL: Sources for " ++ relName name ++ " in " ++ path ++
                       " don't match computed configuration.\n\ncomputed:\n" ++
                       ppDisplay computed ++ "\ninstalled:\n" ++
                       ppDisplay installed
      recreate reason =
          do let root = EnvSet.cleanOS eset
             base <- osBaseDistro <$> getOS
             sources <- sourcesPath (sliceListName base)
             dist <- distDir (sliceListName base)
             liftIO $ do ePutStrLn $ "Removing and recreating build environment at " ++ root ++ ": " ++ show reason
                         -- ePutStrLn ("removeRecursiveSafely " ++ rootPath root)
                         removeRecursiveSafely root
                         -- ePutStrLn ("createDirectoryIfMissing True " ++ show dist)
                         createDirectoryIfMissing True dist
                         -- ePutStrLn ("writeFile " ++ show sources ++ " " ++ show (show . osBaseDistro $ os))
                         replaceFile sources (ppDisplay base)
             rebuildOS (EnvRoot root) distro include exclude components

      doInclude :: (MonadOS m, MonadIO m, MonadMask m) => m ()
      doInclude =
          do aptGetInstall (map (\ s -> (BinPkgName s, Nothing)) include)
             aptGetInstall (map (\ s -> (BinPkgName s, Nothing)) optional) `catch` (\ (e :: IOError) -> ePutStrLn ("Ignoring exception on optional package install: " ++ show e))
      doLocales :: (MonadOS m, MonadIO m) => m ()
      doLocales =
          do os <- getOS
             localeName <- liftIO $ try (getEnv "LANG")
             liftIO $ localeGen os (either (\ (_ :: IOError) -> "en_US.UTF-8") id localeName)

-- | Not used, but could be a substitute for buildOS.
_pbuilderBuild :: (MonadRepos m, MonadTop m, MonadMask m) =>
            EnvRoot
         -> NamedSliceList
         -> LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSImage
_pbuilderBuild root distro repo _extraEssential _omitEssential _extra =
    do top <- askTop
       os <- liftIO $ pbuilder top root distro repo _extraEssential _omitEssential _extra
       putOSImage os
       try (evalMonadOS updateOS root) >>= either (\ (e :: SomeException) -> error (show e)) return
       return os

rebuildOS :: (MonadOS m, MonadRepos m, MonadTop m, MonadMask m) =>
             EnvRoot			-- ^ The location where image is to be built
           -> NamedSliceList		-- ^ The sources.list of the base distribution
           -> [String]			-- ^ Extra packages to install - e.g. keyrings
           -> [String]			-- ^ Packages to exclude
           -> [String]			-- ^ Components of the base repository
           -> m ()
rebuildOS root distro include exclude components =
          do master <- osLocalMaster <$> getOS
             _key <- buildOS root distro master include exclude components
             syncLocalPool

-- | Create a new clean build environment in root.clean FIXME: create
-- an ".incomplete" flag and remove it when build-env succeeds
buildOS :: (MonadRepos m, MonadTop m, MonadMask m) =>
            EnvRoot
         -> NamedSliceList
         -> LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> m OSImage
buildOS root distro repo include exclude components =
    do os <- liftIO $ debootstrap root distro repo include exclude components
       putOSImage os
       evalMonadOS updateOS root
       liftIO $ neuterEnv os
       return os

-- | Try to update an existing build environment: run apt-get update
-- and dist-upgrade.
updateOS :: (MonadOS m, MonadRepos m, MonadMask m) => m ()
updateOS = quieter 1 $ do
  root <- (rootPath . osRoot) <$> getOS
  liftIO $ createDirectoryIfMissing True (root </> "etc")
  liftIO $ readFile "/etc/resolv.conf" >>= writeFile (root </> "etc/resolv.conf")
  liftIO $ prepareDevs root
  syncLocalPool
  verifySources
  -- Disable the starting of services in the changeroot
  _ <- liftIO $ useEnv root (return . force) $ readProcessWithExitCode "dpkg-divert" ["--local", "--rename", "--add", "/sbin/initctl"] ""
  _ <- liftIO $ useEnv root (return . force) $ readProcessWithExitCode "ln" ["-s", "/bin/true", "/sbin/initctl"] ""
  code <- liftIO $ sshCopy root
  case code of
    ExitSuccess -> return ()
    _ -> error $ "sshCopy -> " ++ show code
    where
      verifySources :: (MonadOS m, MonadRepos m) => m ()
      verifySources =
          do root <- osRoot <$> getOS
             computed <- remoteOnly <$> osFullDistro <$> getOS
             let sourcesPath' = rootPath root </> "etc/apt/sources.list"
             text <- liftIO (try $ readFile sourcesPath')
             installed <-
                 case text of
                   Left (_ :: SomeException) -> return Nothing
                   Right s -> verifySourcesList (Just root) (parseSourcesList s) >>= return . Just . remoteOnly
             case installed of
               Nothing -> (osBaseDistro <$> getOS) >>= \ sources -> throw $ Missing (sliceListName sources) sourcesPath'
               Just installed'
                   | installed' /= computed ->
                       (osBaseDistro <$> getOS) >>= \ sources -> throw $ Changed (sliceListName sources) sourcesPath' computed installed'
               _ -> return ()
      remoteOnly :: SliceList -> SliceList
      remoteOnly x = x {slices = filter r (slices x)} where r y = (uriScheme . sourceUri . sliceSource $ y) /= "file:"
{-
    get >>= updateOS' >>= either throw put
    where
      updateOS' :: MonadRepos m => OSImage -> m (Either UpdateError OSImage)
      updateOS' os =
          do let root = getL osRoot os
             liftIO $ createDirectoryIfMissing True (rootPath root ++ "/etc")
             liftIO $ readFile "/etc/resolv.conf" >>= writeFile (rootPath root ++ "/etc/resolv.conf")
             verified <- verifySources os
             case verified of
               Left x -> return $ Left x
               Right _ ->
                   do liftIO $ prepareDevs (rootPath root)
                      evalMonadOS (syncLocalPool >> updateLists) os
                      _ <- liftIO $ sshCopy (rootPath root)
                      source' <- evalMonadOS getSourcePackages' os'
                      binary <- evalMonadOS getBinaryPackages' os'
                      return . Right $ setL osSourcePackages source' $ setL osBinaryPackages binary $ os'
      verifySources :: MonadRepos m => OSImage -> m (Either UpdateError OSImage)
      verifySources os =
          do let root = getL osRoot os
             computed <- remoteOnly <$> evalMonadOS osFullDistro os
             let sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
             text <- liftIO (try $ readFile sourcesPath')
             installed <-
                 case text of
                   Left (_ :: SomeException) -> return Nothing
                   Right s -> verifySourcesList (Just root) (parseSourcesList s) >>= return . Just . remoteOnly
             case installed of
               Nothing -> return $ Left $ Missing (sliceListName (getL osBaseDistro os)) sourcesPath'
               Just installed'
                   | installed' /= computed ->
                       return $ Left $ Changed (sliceListName (getL osBaseDistro os)) sourcesPath' computed installed'
               _ -> return $ Right os
      remoteOnly :: SliceList -> SliceList
      remoteOnly x = x {slices = filter r (slices x)} where r y = (uriScheme . sourceUri . sliceSource $ y) /= "file:"
-}

-- |Prepare a minimal \/dev directory
{-# WARNING prepareDevs "This function should check all the result codes" #-}
prepareDevs :: FilePath -> IO ()
prepareDevs root = do
  mapM_ prepareDev devices
  where
    devices :: [(FilePath, String, Int, Int)]
    devices = [(root ++ "/dev/null", "c", 1, 3),
               (root ++ "/dev/zero", "c", 1, 5),
               (root ++ "/dev/full", "c", 1, 7),
               (root ++ "/dev/console", "c", 5, 1),
               (root ++ "/dev/random", "c", 1, 8),
               (root ++ "/dev/urandom", "c", 1, 9)] ++
              (map (\ n -> (root ++ "/dev/loop" ++ show n, "b", 7, n)) [0..7]) ++
              (map (\ n -> (root ++ "/dev/loop/" ++ show n, "b", 7, n)) [0..7])
    prepareDev (path, typ, major, minor) = do
                     createDirectoryIfMissing True (fst (splitFileName path))
                     let cmd = "mknod " ++ path ++ " " ++ typ ++ " " ++ show major ++ " " ++ show minor ++ " 2> /dev/null"
                     exists <- doesFileExist path
                     case exists of
                       False -> readProcFailing (shell cmd) L.empty >>= return . collectProcessTriple >>= \ (result, _, _) -> return result
                       True -> return ExitSuccess
