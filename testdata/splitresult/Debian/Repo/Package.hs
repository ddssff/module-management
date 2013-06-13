{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package
    ( -- * Source and binary packages
      sourceFilePaths
    , binaryPackageSourceVersion
    , binarySourceVersion
    , sourcePackageBinaryNames
    , sourceBinaryNames
    , toSourcePackage
    , toBinaryPackage
    , binaryPackageSourceID
    , sourcePackageBinaryIDs
    , sourcePackagesOfIndex
    , sourcePackagesOfCachedIndex
    , binaryPackagesOfIndex
    , binaryPackagesOfCachedIndex
    , getPackages
    , putPackages
    , releaseSourcePackages
    , releaseBinaryPackages
    , TH.FixityDirection(..)
    , TH.Fixity(..)
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import Debian.Repo.Package.BinaryPackageSourceID (binaryPackageSourceID)
import Debian.Repo.Package.BinaryPackageSourceVersion (binaryPackageSourceVersion)
import Debian.Repo.Package.BinaryPackagesOfCachedIndex (binaryPackagesOfCachedIndex)
import Debian.Repo.Package.BinaryPackagesOfIndex (binaryPackagesOfIndex)
import Debian.Repo.Package.BinarySourceVersion (binarySourceVersion)
import Debian.Repo.Package.GetPackages (getPackages)
import Debian.Repo.Package.PutPackages (putPackages)
import Debian.Repo.Package.ReleaseBinaryPackages (releaseBinaryPackages)
import Debian.Repo.Package.ReleaseSourcePackages (releaseSourcePackages)
import Debian.Repo.Package.SourceBinaryNames (sourceBinaryNames)
import Debian.Repo.Package.SourceFilePaths (sourceFilePaths)
import Debian.Repo.Package.SourcePackageBinaryIDs (sourcePackageBinaryIDs)
import Debian.Repo.Package.SourcePackageBinaryNames (sourcePackageBinaryNames)
import Debian.Repo.Package.SourcePackagesOfCachedIndex (sourcePackagesOfCachedIndex)
import Debian.Repo.Package.SourcePackagesOfIndex (sourcePackagesOfIndex)
import Debian.Repo.Package.ToBinaryPackage (toBinaryPackage)
import Debian.Repo.Package.ToSourcePackage (toSourcePackage)
import qualified Language.Haskell.TH.Syntax as TH (Fixity(..), FixityDirection(..))
