{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Debian.Repo.State.AptImage
    ( withAptImage
    , aptSourcePackages
    , aptBinaryPackages
    , prepareSource
    ) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadIO(..), MonadTrans(lift))
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Debian.Changes (ChangeLogEntry(logVersion))
import Debian.Pretty (ppDisplay)
import Debian.Relation (SrcPkgName(unSrcPkgName))
import Debian.Release (ReleaseName)
import Debian.Repo.AptImage (aptDir, aptGetSource, aptGetUpdate)
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import Debian.Repo.Internal.Apt (AptImage(aptImageArch, aptImageRoot, aptImageSources,
                                          aptBinaryPackageCache, aptSourcePackageCache),
                                 cacheRootDir, createAptImage, MonadApt(..), modifyApt)
import Debian.Repo.Internal.Repos (AptKey, evalMonadApt, getAptKey, MonadRepos(..), putAptImage)
import Debian.Repo.PackageID (PackageID(packageName), PackageID(packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage(sourcePackageID))
import Debian.Repo.Prelude (symbol)
import Debian.Repo.Prelude.Verbosity (qPutStr, qPutStrLn)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), SliceList, SourcesChangedAction)
import Debian.Repo.SourceTree (DebianBuildTree(debTree'), DebianSourceTree(tree'), HasChangeLog(entry), findDebianBuildTrees, SourceTree(dir'))
import Debian.Repo.State.PackageIndex (binaryPackagesFromSources, sourcePackagesFromSources)
import Debian.Repo.State.Slice (updateCacheSources)
import Debian.Repo.Top (MonadTop)
import Debian.Version (DebianVersion)
import System.Directory (createDirectoryIfMissing)
import System.Unix.Directory (removeRecursiveSafely)

instance MonadApt m => MonadApt (StateT EnvRoot m) where
    getApt = lift getApt
    putApt = lift . putApt

instance MonadRepos m => MonadRepos (StateT AptImage m) where
    getRepos = lift getRepos
    putRepos = lift . putRepos

withAptImage :: (MonadRepos m, MonadTop m) => SourcesChangedAction -> NamedSliceList -> StateT AptImage m a -> m a
withAptImage sourcesChangedAction sources action = prepareAptImage sourcesChangedAction sources >>= evalMonadApt action

-- |Create a skeletal enviroment sufficient to run apt-get.
prepareAptImage :: forall m. (MonadTop m, MonadRepos m) =>
                 SourcesChangedAction	-- What to do if environment already exists and sources.list is different
              -> NamedSliceList		-- The sources.list
              -> m AptKey		-- The resulting environment
prepareAptImage sourcesChangedAction sources = do
  mkey <- getAptKey =<< cacheRootDir (sliceListName sources)
  maybe (prepareAptImage' sourcesChangedAction sources) return mkey

prepareAptImage' :: forall m. (MonadCatch m, MonadTop m, MonadRepos m) => SourcesChangedAction -> NamedSliceList -> m AptKey
prepareAptImage' sourcesChangedAction sources = do
  mkey <- getAptKey =<< cacheRootDir (sliceListName sources)
  maybe (prepareAptImage'' `catch` handle) return mkey
    where
      handle :: SomeException -> m AptKey
      handle e = do
        qPutStrLn ("Exception preparing " ++ (ppDisplay . sliceListName $ sources) ++ ": " ++ show e)
        removeAptImage
        prepareAptImage''
      prepareAptImage'' = do
        qPutStrLn ($(symbol 'prepareAptImage) ++ ": " ++ (ppDisplay . sliceListName $ sources))
        key <- putAptImage =<< createAptImage sources
        evalMonadApt (updateCacheSources sourcesChangedAction sources >> aptGetUpdate) key
        return key
      removeAptImage = cacheRootDir (sliceListName sources) >>= liftIO . removeRecursiveSafely . rootPath

-- |Run apt-get update and then retrieve all the packages referenced
-- by the sources.list.  The source packages are sorted so that
-- packages with the same name are together with the newest first.
-- updateAptEnv :: (MonadRepos m, MonadApt m) => m ()
-- updateAptEnv = aptGetUpdate

aptSourcePackages :: (MonadRepos m, MonadApt m) => m [SourcePackage]
aptSourcePackages = do
  mpkgs <- aptSourcePackageCache <$> getApt
  maybe aptSourcePackages' return mpkgs
    where
      aptSourcePackages' = do
        root <- aptImageRoot <$> getApt
        arch <- aptImageArch <$> getApt
        sources <- aptImageSources <$> getApt
        -- qPutStrLn ($(symbol 'aptSourcePackages) ++ " " ++ show (pretty (sliceListName sources)))
        pkgs <- sourcePackagesFromSources root arch (sliceList sources)
        modifyApt (\ s -> s {aptSourcePackageCache = Just pkgs})
        return pkgs

aptBinaryPackages :: (MonadRepos m, MonadApt m) => m [BinaryPackage]
aptBinaryPackages = do
  mpkgs <- aptBinaryPackageCache <$> getApt
  maybe aptBinaryPackages' return mpkgs
    where
      aptBinaryPackages' = do
        root <- aptImageRoot <$> getApt
        arch <- aptImageArch <$> getApt
        sources <- aptImageSources <$> getApt
        -- qPutStrLn ($(symbol 'aptBinaryPackages) ++ " " ++ show (pretty (sliceListName sources)))
        pkgs <- binaryPackagesFromSources root arch (sliceList sources)
        modifyApt (\ s -> s {aptBinaryPackageCache = Just pkgs})
        return pkgs
{-
    do qPutStrLn "AptImage.getBinaryPackages"
       root <- getL aptImageRoot <$> getApt
       arch <- getL aptImageArch <$> getApt
       sources <- (sliceList . getL aptImageSources) <$> getApt
       binaryPackagesFromSources root arch sources
-}

data UpdateError
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, ppDisplay l1, ppDisplay l2]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"

-- |Retrieve a source package via apt-get.
prepareSource :: (MonadRepos m, MonadApt m, MonadTop m, MonadIO m) =>
                 SrcPkgName			-- The name of the package
              -> Maybe DebianVersion		-- The desired version, if Nothing get newest
              -> m DebianBuildTree		-- The resulting source tree
prepareSource package version =
    do root <- (rootPath . aptImageRoot) <$> getApt
       dir <- aptDir package
       liftIO $ createDirectoryIfMissing True dir
       ready <- liftIO $ findDebianBuildTrees dir
       version' <- latestVersion package version
       case (version', ready) of
         (Nothing, _) ->
             fail $ "No available versions of " ++ unSrcPkgName package ++ " in " ++ root
         (Just requested, [tree])
             | requested == (logVersion . entry $ tree) ->
                 return tree
         (Just requested, []) ->
             do aptGetSource dir [(package, Just requested)]
                trees <- liftIO $ findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> fail $ "apt-get source failed in " ++ dir ++ " (1): trees=" ++ show (map (dir' . tree' . debTree') trees)
         (Just requested, _) ->
             do -- One or more incorrect versions are available, remove them
                liftIO $ removeRecursiveSafely dir
                qPutStr $ "Retrieving APT source for " ++ unSrcPkgName package
                aptGetSource dir [(package, Just requested)]
                trees <- liftIO $ findDebianBuildTrees dir
                case trees of
                  [tree] -> return tree
                  _ -> fail $ "apt-get source failed (2): trees=" ++ show (map (dir' . tree' . debTree') trees)

-- | Find the most recent version of a source package.
latestVersion :: (MonadRepos m, MonadApt m) => SrcPkgName -> Maybe DebianVersion -> m (Maybe DebianVersion)
latestVersion package exact = do
  pkgs <- aptSourcePackages
  let versions = map (packageVersion . sourcePackageID) $ filter ((== package) . packageName . sourcePackageID) $ pkgs
      newest = listToMaybe $ reverse $ sort $ versions
  return $ maybe newest Just exact
