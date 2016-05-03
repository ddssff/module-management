{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.AptImage
    ( AptImage(..)
    ) where

import Debian.Arch (Arch(..))
import Debian.Release (ReleaseName(..))
import Debian.Repo.Types.AptCache (AptCache(..))
import Debian.Repo.Types.EnvPath (EnvRoot)
import Debian.Repo.Types.PackageIndex (BinaryPackage, SourcePackage)
import qualified Debian.Repo.Types.Slice as T (SliceList)

{-
instance Show FileStatus where
    show _ = "def :: FileStatus"
-}
---------------------- CACHED OS IMAGE ---------------------

data AptImage =
    AptImage { aptGlobalCacheDir :: FilePath
             , aptImageRoot :: EnvRoot
             , aptImageArch :: Arch
             , aptImageSliceList :: T.SliceList
             , aptImageReleaseName :: ReleaseName
             , aptImageSourcePackages :: [SourcePackage]
             , aptImageBinaryPackages :: [BinaryPackage]
             }

instance Show AptImage where
    show apt = "AptImage " ++ relName (aptImageReleaseName apt)

instance AptCache AptImage where
    globalCacheDir = aptGlobalCacheDir
    rootDir = aptImageRoot
    aptArch = aptImageArch
    aptBaseSliceList = aptImageSliceList
    aptSourcePackages = aptImageSourcePackages
    aptBinaryPackages = aptImageBinaryPackages
    aptReleaseName = aptImageReleaseName

instance Ord AptImage where
    compare a b = compare (aptImageReleaseName a) (aptImageReleaseName b)

instance Eq AptImage where
    a == b = compare a b == EQ
