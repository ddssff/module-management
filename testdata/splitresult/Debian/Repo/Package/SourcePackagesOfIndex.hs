{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.SourcePackagesOfIndex
    ( -- * Source and binary packages
      sourcePackagesOfIndex
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import Debian.Repo.Package.GetPackages (getPackages)
import Debian.Repo.Package.ToSourcePackage (toSourcePackage)
import Control.Exception as E (SomeException)
import "mtl" Control.Monad.Trans (MonadIO(..))
import Data.List as List (map)
import Debian.Arch (Arch(Source))
import Debian.Repo.Types.PackageIndex (BinaryPackage(packageInfo), PackageIndex(packageIndexArch), SourcePackage)
import Debian.Repo.Types.Release (Release)
import Debian.Repo.Types.Repo (RepoKey)
import Debian.Repo.Types.Repository (MonadRepoCache)

sourcePackagesOfIndex :: MonadRepoCache m => RepoKey -> Release -> PackageIndex -> m (Either SomeException [SourcePackage])
sourcePackagesOfIndex repo release index =
    case packageIndexArch index of
      Source -> liftIO (getPackages repo release index) >>= return . either Left (Right . List.map (toSourcePackage index . packageInfo))
      _ -> return (Right [])

-- FIXME: assuming the index is part of the cache

