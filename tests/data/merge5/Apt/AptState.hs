{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PackageImports, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Apt.AptState
    ( AptState(AptState, aptImageMap, binaryPackageMap, releaseMap,
         repoMap, sourcePackageMap)
    ) where

import qualified Data.Map as Map (Map)
import Debian.Release (ReleaseName)
import Debian.Repo.Internal.Apt (AptImage)
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Release (Release)
import Debian.Repo.RemoteRepository (RemoteRepository)
import Debian.Repo.Repo (RepoKey)
import Debian.Sources (SliceName)
import System.Posix.Files (FileStatus)

-- | This represents the state of the IO system.
data AptState
    = AptState
      { repoMap :: Map.Map RepoKey RemoteRepository		-- ^ Map to look up known Repository objects
      , releaseMap :: Map.Map (RepoKey, ReleaseName) Release -- ^ Map to look up known Release objects
      , aptImageMap :: Map.Map SliceName AptImage	-- ^ Map to look up prepared AptImage objects
      , sourcePackageMap :: Map.Map FilePath (FileStatus, [SourcePackage])
      , binaryPackageMap :: Map.Map FilePath (FileStatus, [BinaryPackage])
      }
