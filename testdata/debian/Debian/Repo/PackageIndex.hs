-- | The 'PackageIndex' type is the result of parsing a file
-- containing control information about debian packages, either source
-- or binary.  Examples of such files include
-- @http://ftp.debian.org/debian/dists/sid/main/source/Sources.bz2@ or
-- @http://ftp.debian.org/debian/dists/sid/main/binary-amd64/Packages.bz2@.
{-# LANGUAGE FlexibleInstances, OverloadedStrings, StandaloneDeriving #-}
module Debian.Repo.PackageIndex
    ( PackageIndex(..)

    , BinaryPackage(..)
    , prettyBinaryPackage

    , SourcePackage(..)
    , SourceControl(..)
    , SourceFileSpec(..)

    , packageIndexName
    , packageIndexPath
    , packageIndexDir
    , packageIndexPaths
    , packageIndexDirs
    , packageIndexes
    , sourceIndexes
    , binaryIndexes
    , releaseDir

    , sortSourcePackages
    , sortBinaryPackages
    ) where

import Data.List as List (sortBy, map, filter)
import Data.Monoid ((<>))
import Data.Set as Set (map, filter, toList, unions)
import Data.Text (Text)
import Debian.Arch (Arch(..), ArchOS(..), ArchCPU(..), prettyArch)
import qualified Debian.Control.Text as T
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import qualified Debian.Relation as B (Relations)
import Debian.Pretty (PP(..), ppPrint)
import Debian.Release (releaseName', Section(..), sectionName')
import Debian.Repo.PackageID (PackageID(packageName, packageVersion), prettyPackageID)
import Debian.Repo.Release (Release(..))
import System.FilePath ((</>))
import System.Posix.Types (FileOffset)
import Text.PrettyPrint.HughesPJClass (Doc, Pretty(pPrint), text)

-- | A package index is identified by Section (e.g. main, contrib,
-- non-free) and architecture (e.g. source, i386, amd64.)
data PackageIndex
    = PackageIndex { packageIndexComponent :: Section
                   , packageIndexArch :: Arch
                   } deriving (Eq, Ord, Show)

instance Pretty (PP PackageIndex) where
    pPrint (PP x) = ppPrint (packageIndexComponent x) <> text "_" <> ppPrint (packageIndexArch x)

instance Pretty (PP Section) where
    pPrint (PP (Section x)) = pPrint x

instance Pretty (PP Arch) where
    pPrint (PP Source) = text "source"
    pPrint (PP All) = text "all"
    pPrint (PP (Binary ArchOSAny ArchCPUAny)) = text "any"
    pPrint (PP (Binary (ArchOS os) ArchCPUAny)) = ppPrint os
    pPrint (PP (Binary ArchOSAny (ArchCPU cpu))) = ppPrint cpu
    pPrint (PP (Binary (ArchOS os) (ArchCPU cpu))) = ppPrint (os <> "-" <> cpu)

{-
instance PackageVersion BinaryPackage where
    pkgName = binaryPackageName
    pkgVersion = packageVersion . packageID
-}

-- | The 'BinaryPackage' type adds to the 'PackageID' type the control
-- information obtained from the package index.
data BinaryPackage
    = BinaryPackage
      { packageID :: PackageID BinPkgName
      , packageInfo :: T.Paragraph
      , pDepends :: B.Relations
      , pPreDepends :: B.Relations
      , pConflicts ::B.Relations
      , pReplaces :: B.Relations
      , pProvides :: B.Relations
      } deriving (Show)

-- In a perfect world these would be valid Eq and Ord instances, but
-- they are not because different builds of the same version can occur
-- on different machines that are unaware of each other.  These should
-- be removed.  But they are currently in use.

instance Ord BinaryPackage where
    compare a b = compare (packageID a) (packageID b)

instance Eq BinaryPackage where
    a == b = (packageID a) == (packageID b)

prettyBinaryPackage :: BinaryPackage -> Doc
prettyBinaryPackage = prettyPackageID . packageID

data SourcePackage
    = SourcePackage
      { sourcePackageID :: PackageID SrcPkgName
      , sourceParagraph :: T.Paragraph
      , sourceControl :: SourceControl
      , sourceDirectory :: String
      , sourcePackageFiles :: [SourceFileSpec]
      } deriving (Show, Eq, Ord)

-- |Source package information derived from the control paragraph.
data SourceControl
    = SourceControl
      { source :: Text
      , maintainer :: NameAddr
      , uploaders :: [NameAddr]
      , packageSection :: Maybe Section' -- Should this be the same type as the Section field in a .changes file?
      , packagePriority :: Maybe Priority
      , buildDepends :: [Package]
      , buildDependsIndep :: [Package]
      , buildConflicts :: [Package]
      , buildConflictsIndep :: [Package]
      , standardsVersion :: Maybe StandardsVersion -- There are packages that don't have this
      , homepage :: Maybe Text -- There are packages that don't have this
      } deriving (Show, Eq, Ord)

type NameAddr = Text
type StandardsVersion = Text
type Section' = Text
type Priority = Text
type Package = Text

data SourceFileSpec
    = SourceFileSpec
      { sourceFileMD5sum :: String
      , sourceFileSize :: FileOffset
      , sourceFileName :: FilePath
      }
    deriving (Show, Eq, Ord)

packageIndexName :: PackageIndex -> FilePath
packageIndexName index =
    case packageIndexArch index of
      Source -> "Sources"
      _ -> "Packages"

packageIndexPath :: Release -> PackageIndex -> FilePath
packageIndexPath release index = packageIndexDir release index </> packageIndexName index

packageIndexDir :: Release -> PackageIndex -> FilePath
packageIndexDir release index =
    case packageIndexArch index of
      Source -> releaseDir release </> sectionName' (packageIndexComponent index) </> "source"
      _ -> (releaseDir release </> sectionName' (packageIndexComponent index) </>
            -- Will prettyArch give us linux-amd64 when we just want amd64?
            "binary-" ++ show (prettyArch (packageIndexArch index)))

releaseDir :: Release -> String
releaseDir release = "dists" </> (releaseName' . releaseName $ release)

packageIndexPaths :: Release -> [FilePath]
packageIndexPaths release = List.map (packageIndexPath release) . packageIndexes $ release

packageIndexDirs :: Release -> [FilePath]
packageIndexDirs release = List.map (packageIndexDir release) . packageIndexes $ release

packageIndexes :: Release -> [PackageIndex]
packageIndexes release = sourceIndexes release ++ binaryIndexes release

sourceIndexes :: Release -> [PackageIndex]
sourceIndexes release =
    List.map componentIndex (releaseComponents $ release)
    where componentIndex component = PackageIndex { packageIndexComponent = component
                                                  , packageIndexArch = Source }

binaryIndexes :: Release -> [PackageIndex]
binaryIndexes release =
    toList . unions . List.map componentIndexes $ releaseComponents release
    where
      --componentIndexes :: Section -> [PackageIndex]
      componentIndexes component =
          Set.map archIndex (Set.filter (/= Source) (releaseArchitectures release))
          where
            --archIndex :: Arch -> PackageIndex
            archIndex arch = PackageIndex { packageIndexComponent = component
                                          , packageIndexArch = arch }

-- | Return a sorted list of available source packages, newest version first.
sortSourcePackages :: [SrcPkgName] -> [SourcePackage] -> [SourcePackage]
sortSourcePackages names pkgs =
    sortBy cmp . filterNames $ pkgs
    where
      filterNames :: [SourcePackage] -> [SourcePackage]
      filterNames packages =
          List.filter (flip elem names . packageName . sourcePackageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . sourcePackageID $ p1
            v2 = packageVersion . sourcePackageID $ p2

sortBinaryPackages :: [BinPkgName] -> [BinaryPackage] -> [BinaryPackage]
sortBinaryPackages names pkgs =
    sortBy cmp . filterNames $ pkgs
    where
      filterNames :: [BinaryPackage] -> [BinaryPackage]
      filterNames packages =
          List.filter (flip elem names . packageName . packageID) packages
      cmp p1 p2 =
          compare v2 v1		-- Flip args to get newest first
          where
            v1 = packageVersion . packageID $ p1
            v2 = packageVersion . packageID $ p2
