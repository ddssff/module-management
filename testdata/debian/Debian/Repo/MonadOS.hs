{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.MonadOS
    ( MonadOS(getOS, putOS, modifyOS)
    , evalMonadOS
    , updateLists
    , withProc
    , withTmp
    , aptGetInstall
    , syncLocalPool
    , osFlushPackageCache
    , buildEssential
    , Debian.Repo.MonadOS.syncOS
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (force)
import Control.Exception (evaluate, SomeException, catch)
import Control.Monad.Catch (bracket, MonadCatch, MonadMask, throwM, try)
import Control.Monad.State (MonadState(get), StateT, evalStateT, get)
import Control.Monad.Trans (liftIO, MonadIO, lift)
import Data.ByteString.Lazy as L (empty)
import Data.Time (NominalDiffTime)
import Debian.Pretty (ppDisplay)
import Debian.Relation (PkgName, Relations)
import Debian.Repo.EnvPath (EnvPath(EnvPath, envPath, envRoot), EnvRoot(rootPath))
import Debian.Repo.Internal.Repos (MonadRepos, osFromRoot, putOSImage, syncOS)
import Debian.Repo.LocalRepository (copyLocalRepo)
import Debian.Repo.OSImage as OS (OSImage(osRoot, osLocalMaster, osLocalCopy, osSourcePackageCache, osBinaryPackageCache))
import qualified Debian.Repo.OSImage as OS (buildEssential)
import Debian.Repo.Prelude.Verbosity (quieter, timeTask, readProcFailing, ePutStrLn)
import Debian.Repo.Top (MonadTop)
import Debian.Version (DebianVersion, prettyDebianVersion)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath ((</>))
import System.Process (proc)
import System.Process.Chunks (collectProcessTriple)
import System.Unix.Chroot (useEnv)

-- | The problem with having an OSImage in the state of MonadOS is
-- that then we are modifying a copy of the OSImage in MonadRepos, we
-- want to go into MonadRepos and modify the map element there.  So
-- instead put an EnvRoot to look up the OSImage.
class (Monad m, Functor m) => MonadOS m where
    getOS :: m OSImage
    putOS :: OSImage -> m ()
    modifyOS :: (OSImage -> OSImage) -> m ()

instance MonadRepos m => MonadOS (StateT EnvRoot m) where
    getOS = get >>= \ root -> maybe (error "getOS") id <$> (lift $ osFromRoot root)
    putOS = lift . putOSImage
    modifyOS f = getOS >>= putOS . f

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadOS :: MonadRepos m => StateT EnvRoot m a -> EnvRoot -> m a
evalMonadOS task root = do
  a <- evalStateT task root
  return a

-- | Run @apt-get update@ and @apt-get dist-upgrade@.  If @update@
-- fails, run @dpkg --configure -a@ before running @dist-upgrade@.
updateLists :: (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m NominalDiffTime
updateLists = quieter 1 $
    do root <-rootPath . osRoot <$> getOS
       withProc $ liftIO $ do
         (code, _, _) <- useEnv root forceList (readProcFailing update "") >>= return . collectProcessTriple
         _ <- case code of
                ExitFailure _ ->
                    do _ <- useEnv root forceList (readProcFailing configure "")
                       useEnv root forceList (readProcFailing update "")
                _ -> return []
         (_, elapsed) <- timeTask (useEnv root forceList (readProcFailing upgrade ""))
         return elapsed
    where
       update = proc "apt-get" ["update"]
       configure = proc "dpkg" ["--configure", "-a"]
       upgrade = proc "apt-get" ["-y", "--force-yes", "dist-upgrade"]

-- | Do an IO task in the build environment with /proc mounted.
withProc :: forall m c. (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m c -> m c
withProc task =
    do root <- rootPath . osRoot <$> getOS
       let proc' = root </> "proc"
           sys = root </> "sys"
           pre :: m ()
           mountProc = proc "mount" ["--bind", "/proc", proc']
           mountSys = proc "mount" ["--bind", "/sys", sys]
           umountProc = proc "umount" [proc']
           umountSys = proc "umount" [sys]
           umountProcLazy = proc "umount" ["-l", proc']
           umountSysLazy = proc "umount" ["-l", sys]

           pre = liftIO (do createDirectoryIfMissing True proc'
                            readProcFailing mountProc L.empty
                            createDirectoryIfMissing True sys
                            readProcFailing mountSys L.empty
                            return ())
           post :: () -> m ()
           post _ = liftIO $ do readProcFailing umountProc L.empty
                                  `catch` (\ (e :: IOError) ->
                                               ePutStrLn ("Exception unmounting proc, trying lazy: " ++ show e) >>
                                               readProcFailing umountProcLazy L.empty)
                                readProcFailing umountSys L.empty
                                  `catch` (\ (e :: IOError) ->
                                               ePutStrLn ("Exception unmounting sys, trying lazy: " ++ show e) >>
                                               readProcFailing umountSysLazy L.empty)
                                return ()
           task' :: () -> m c
           task' _ = task
       bracket pre post task'

-- | Do an IO task in the build environment with /proc mounted.
withTmp :: forall m c. (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => m c -> m c
withTmp task =
    do root <- rootPath . osRoot <$> getOS
       let dir = root </> "tmp"
           mountTmp = proc "mount" ["--bind", "/tmp", dir]
           umountTmp = proc "umount" [dir]
           pre :: m ()
           pre = liftIO $ do createDirectoryIfMissing True dir
                             readProcFailing mountTmp L.empty
                             return ()
           post :: () -> m ()
           post _ = liftIO $ readProcFailing umountTmp L.empty >> return ()
           task' :: () -> m c
           task' _ = try task >>= either (\ (e :: SomeException) -> throwM e) return
       bracket pre post task'

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetInstall :: (MonadOS m, MonadIO m, PkgName n) => [(n, Maybe DebianVersion)] -> m ()
aptGetInstall packages =
    do root <- rootPath . osRoot <$> getOS
       liftIO $ useEnv root (return . force) $ do
         readProcFailing p L.empty
         return ()
    where
      p = proc "apt-get" args'
      args' = ["-y", "--force-yes", "install"] ++ map formatPackage packages
      formatPackage (name, Nothing) = ppDisplay name
      formatPackage (name, Just version) = ppDisplay name ++ "=" ++ show (prettyDebianVersion version)

-- | This is a deepseq thing
forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output

-- | Use rsync to synchronize the pool of locally built packages from
-- outside the build environment to the location inside the
-- environment where apt can see and install the packages.  On the
-- assumption that we are doing this because the pool changed, we also
-- flush the cached package lists.
syncLocalPool :: (MonadIO m, MonadOS m, MonadCatch m, MonadMask m) => m ()
syncLocalPool =
    do os <- getOS
       repo' <- copyLocalRepo (EnvPath {envRoot = osRoot os, envPath = "/work/localpool"}) (osLocalMaster os)
       putOS (os {osLocalCopy = repo'})
       updateLists
       -- Presumably we are doing this because the pool changed, and
       -- that invalidates the OS package lists.
       osFlushPackageCache

osFlushPackageCache :: MonadOS m => m ()
osFlushPackageCache = modifyOS (\ os -> os {osSourcePackageCache = Nothing, osBinaryPackageCache = Nothing})

-- | Get the version of the newest ghc available in a build environment.
-- ghcNewestAvailableVersion :: (MonadIO m, Functor m, MonadState OSImage m) => m (Maybe DebianVersion)
-- ghcNewestAvailableVersion = do
--   root <- rootPath . osRoot <$> get
--   liftIO $ GHC.ghcNewestAvailableVersion root

buildEssential :: (MonadOS m, MonadIO m) => m Relations
buildEssential = getOS >>= liftIO . OS.buildEssential

syncOS :: (MonadOS m, MonadTop m, MonadRepos m) => EnvRoot -> m ()
syncOS dstRoot =
    do srcOS <- getOS
       dstOS <- Debian.Repo.Internal.Repos.syncOS srcOS dstRoot
       putOS dstOS
