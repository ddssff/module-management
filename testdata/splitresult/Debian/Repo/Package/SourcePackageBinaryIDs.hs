{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.SourcePackageBinaryIDs
    ( sourcePackageBinaryIDs
    ) where


import Data.List as List (map)
import qualified Data.Text as T (unpack)
import Debian.Arch (Arch(Source))
import Debian.Control (formatParagraph)
import qualified Debian.Control.Text as B (fieldValue)
import Debian.Relation (BinPkgName(..))
import Debian.Repo.Types.PackageIndex (makeBinaryPackageID, PackageID, PackageIndex, SourcePackage(sourceParagraph))
import Debian.Version (parseDebianVersion)
import Text.Regex (mkRegex, splitRegex)

sourcePackageBinaryIDs :: Arch -> PackageIndex -> SourcePackage -> [PackageID BinPkgName]
sourcePackageBinaryIDs Source _ _ = error "invalid argument"
sourcePackageBinaryIDs arch sourceIndex package =
    case (B.fieldValue "Version" info, B.fieldValue "Binary" info) of
      (Just version, Just names) -> List.map (binaryID (parseDebianVersion (T.unpack version))) $ splitRegex (mkRegex "[ ,]+") (T.unpack names)
      _ -> error ("Source package info has no 'Binary' field:\n" ++ (T.unpack . formatParagraph $ info))
    where
      -- Note that this version number may be wrong - we need to
      -- look at the Source field of the binary package info.
      binaryID version name = makeBinaryPackageID name version
      -- binaryIndex = sourceIndex { packageIndexArch = arch }
      info = sourceParagraph package

-- | Get the contents of a package index

