{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |The AptImage object represents a partial OS image which is capable
-- of running apt-get, and thus obtaining repository info and source
-- code packages.
module Debian.Repo.AptImage
    ( prepareAptEnv
    , updateAptEnv
    ) where

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as L (empty)
import Data.Function (on)
import Data.List (sortBy)
import Debian.Release (ReleaseName(..))
import Debian.Repo.AptCache (aptOpts, buildArchOfRoot, cacheRootDir, getBinaryPackagesBase, getSourcePackagesBase, SourcesChangedAction, updateCacheSources)
import Debian.Repo.Monads.Apt (insertAptImage, lookupAptImage, MonadApt(getApt, putApt))
import Debian.Repo.Types.AptImage (AptImage(..))
import Debian.Repo.Types.PackageIndex (PackageID(packageVersion), SourcePackage(sourcePackageID))
import Debian.Repo.Types.Repo (EnvRoot(..), NamedSliceList(sliceList, sliceListName))
import Debian.Sources (SliceName(sliceName))
import Extra.Files (replaceFile, writeFileIfMissing)
import System.Directory (createDirectoryIfMissing)
import System.Process (shell)
import System.Process.Progress (qPutStrLn, quieter, runProcessF)
import Text.PrettyPrint.ANSI.Leijen (pretty)

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
