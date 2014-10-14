{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Internal.Apt
    ( AptImage(..)
    , MonadApt(getApt, putApt)
    , modifyApt
    , cacheRootDir
    , createAptImage
    ) where

import Control.Applicative ((<$>))
import Control.Category ((.))
import Control.Monad.State (MonadState(get, put), StateT)
import Control.Monad.Trans (liftIO, MonadIO)
import Debian.Arch (Arch)
import Debian.Pretty (ppDisplay)
import Debian.Release (ReleaseName(relName))
import Debian.Repo.EnvPath (EnvRoot(EnvRoot, rootPath))
import Debian.Repo.Internal.IO (buildArchOfRoot)
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Prelude (replaceFile, writeFileIfMissing)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName))
import Debian.Repo.Top (dists, MonadTop)
import Prelude hiding ((.))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data AptImage =
    AptImage { aptImageRoot :: EnvRoot
             , aptImageArch :: Arch
             , aptImageSources :: NamedSliceList
             , aptSourcePackageCache :: Maybe [SourcePackage]
             , aptBinaryPackageCache :: Maybe [BinaryPackage]
             }

class (Monad m, Functor m) => MonadApt m where
    getApt :: m AptImage
    putApt :: AptImage -> m ()

modifyApt :: MonadApt m => (AptImage -> AptImage) -> m ()
modifyApt f = getApt >>= putApt . f

instance (Monad m, Functor m) => MonadApt (StateT AptImage m) where
    getApt = get
    putApt = put

instance Show AptImage where
    show apt = "AptImage " ++ relName (sliceListName (aptImageSources apt))

instance Ord AptImage where
    compare a b = compare (sliceListName . aptImageSources $ a) (sliceListName . aptImageSources $ b)

instance Eq AptImage where
    a == b = compare a b == EQ

createAptImage :: (MonadTop m, MonadIO m) => NamedSliceList -> m AptImage
createAptImage sources = do
  root <- cacheRootDir (sliceListName sources)
  liftIO $ do
    arch <- buildArchOfRoot
    let apt = AptImage { aptImageRoot = root
                       , aptImageArch = arch
                       , aptImageSources = sources
                       , aptSourcePackageCache = Nothing
                       , aptBinaryPackageCache = Nothing }

    --vPutStrLn 2 $ "prepareAptEnv " ++ sliceName (sliceListName sources)
    createDirectoryIfMissing True (rootPath root ++ "/var/lib/apt/lists/partial")
    createDirectoryIfMissing True (rootPath root ++ "/var/lib/apt/lists/partial")
    createDirectoryIfMissing True (rootPath root ++ "/var/cache/apt/archives/partial")
    createDirectoryIfMissing True (rootPath root ++ "/var/lib/dpkg")
    createDirectoryIfMissing True (rootPath root ++ "/etc/apt")
    writeFileIfMissing True (rootPath root ++ "/var/lib/dpkg/status") ""
    writeFileIfMissing True (rootPath root ++ "/var/lib/dpkg/diversions") ""
    -- We need to create the local pool before updating so the
    -- sources.list will be valid.
    let sourceListText = ppDisplay (sliceList sources)
    -- ePut ("writeFile " ++ (root ++ "/etc/apt/sources.list") ++ "\n" ++ sourceListText)
    replaceFile (rootPath root ++ "/etc/apt/sources.list") sourceListText
    return apt

cacheRootDir :: MonadTop m => ReleaseName -> m EnvRoot
cacheRootDir release = EnvRoot . (</> relName release </> "aptEnv") <$> dists
