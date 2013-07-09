{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.AptBuildCache
    ( AptBuildCache(..)
    ) where

import Debian.Arch (Arch(..))
import Debian.Release (ReleaseName(..))
import Debian.Repo.Types.AptCache (AptCache)
import Debian.Repo.Types.Common (EnvRoot)
import Debian.Repo.Types.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Types.Common (SliceList)

-- | An apt cache with extra sources.list lines for a local upload repository.
class AptCache t => AptBuildCache t where
    -- | The sources.list
    aptSliceList :: t -> SliceList
