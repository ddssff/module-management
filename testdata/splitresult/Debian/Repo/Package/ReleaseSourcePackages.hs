{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.ReleaseSourcePackages
    ( releaseSourcePackages
    ) where

import Control.Exception as E (SomeException)
import Data.Either (partitionEithers)
import Data.List as List (intercalate, map)
import Data.Set as Set (fromList, Set, unions)
import Debian.Repo.Package.SourcePackagesOfIndex (sourcePackagesOfIndex)
import Debian.Repo.PackageIndex (sourceIndexList)
import Debian.Repo.Types.PackageIndex (SourcePackage)
import Debian.Repo.Types.Release (Release)
import Debian.Repo.Types.Repo (RepoKey)
import Debian.Repo.Types.Repository (MonadRepoCache)

releaseSourcePackages :: MonadRepoCache m => RepoKey -> Release -> m (Set SourcePackage)
releaseSourcePackages repo release =
    mapM (sourcePackagesOfIndex repo release) (sourceIndexList release) >>= return . test
    where
      test :: [Either SomeException [SourcePackage]] -> Set SourcePackage
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Return a list of all the binary packages for all supported architectures.

