{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.BinaryPackageSourceVersion
    ( binaryPackageSourceVersion
    ) where

import Debian.Repo.Package.Internal.BinarySourceVersion (binarySourceVersion')
import Debian.Repo.Types.PackageIndex (BinaryPackage(packageID, packageInfo), binaryPackageName, PackageID(packageVersion))
import Debian.Version (DebianVersion)

-- | Return the name and version number of the source package that
-- generated this binary package.
binaryPackageSourceVersion :: BinaryPackage -> Maybe (String, DebianVersion)
binaryPackageSourceVersion package =
    let binaryName = binaryPackageName package
        binaryVersion = packageVersion . packageID $ package in
    binarySourceVersion' binaryName binaryVersion (packageInfo package)
