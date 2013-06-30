{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.BinaryPackageSourceID
    ( binaryPackageSourceID
    ) where


import qualified Data.Text as T (unpack)
import qualified Debian.Control.Text as B (fieldValue)
import Debian.Relation (BinPkgName(..))
import Debian.Repo.Types.PackageIndex (BinaryPackage(packageID, packageInfo), makeBinaryPackageID, PackageID(..), PackageIndex(PackageIndex))
import Debian.Version (parseDebianVersion)
import Text.Regex (matchRegex, mkRegex)

-- | Parse the /Source/ field of a binary package's control
-- information, this may specify a version number for the source
-- package if it differs from the version number of the binary
-- package.
binaryPackageSourceID :: PackageIndex -> BinaryPackage -> PackageID BinPkgName
binaryPackageSourceID (PackageIndex component _) package =
    case maybe Nothing (matchRegex re . T.unpack) (B.fieldValue "Source" (packageInfo package)) of
      Just [name, _, ""] -> makeBinaryPackageID name (packageVersion pid)
      Just [name, _, version] -> makeBinaryPackageID name (parseDebianVersion version)
      _ -> error "Missing Source attribute in binary package info"
    where
      -- sourceIndex = PackageIndex component Source
      pid = packageID package
      re = mkRegex "^[ ]*([^ (]*)[ ]*(\\([ ]*([^ )]*)\\))?[ ]*$"
