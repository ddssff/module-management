{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.ReleaseBinaryPackages
    ( releaseBinaryPackages
    ) where

import Data.Either (partitionEithers)
import Data.List as List (intercalate, map)
import Data.Set as Set (fromList, Set, unions)
import Debian.Repo.Package.BinaryPackagesOfIndex (binaryPackagesOfIndex)
import Debian.Repo.PackageIndex (binaryIndexList)
import Debian.Repo.Types.PackageIndex (BinaryPackage)
import Debian.Repo.Types.Release (Release)
import Debian.Repo.Types.Repo (RepoKey)
import Debian.Repo.Types.Repository (MonadRepoCache)

releaseBinaryPackages :: MonadRepoCache m => RepoKey -> Release -> m (Set BinaryPackage)
releaseBinaryPackages repo release =
    mapM (binaryPackagesOfIndex repo release) (binaryIndexList release) >>= return . test
    where
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Write a set of packages into a package index.

