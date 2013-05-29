{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.IndexCacheFile
    ( -- * Source and binary packages
      indexCacheFile
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import Debian.Repo.Package.IndexPrefix (indexPrefix)
import Debian.Arch (Arch(..), prettyArch)
import Debian.Repo.Types.AptCache (AptCache(aptArch))
import Debian.Repo.Types.PackageIndex (PackageIndex(packageIndexArch))
import Debian.Repo.Types.Release (Release)
import Debian.Repo.Types.Repo (RepoKey)

indexCacheFile :: (AptCache a) => a -> RepoKey -> Release -> PackageIndex -> FilePath
indexCacheFile apt repo release index =
    case (aptArch apt, packageIndexArch index) of
      (Binary _ _, Source) -> indexPrefix repo release index ++ "_source_Sources"
      (Binary _ _, arch@(Binary _ _)) -> indexPrefix repo release index ++ "_binary-" ++ show (prettyArch arch) ++ "_Packages"
      (x, _) -> error "Invalid build architecture: " ++ show x


