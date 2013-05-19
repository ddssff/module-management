{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.PackageIndex
    ( PackageIndex(..)
    , PackageIndexLocal
    , PackageID(packageVersion)
    , prettyPackageID
    , BinaryPackage(..)
    , binaryPackageName
    , prettyBinaryPackage
    , makeBinaryPackageID
    , SourcePackage(..)
    , sourcePackageName
    , makeSourcePackageID
    , SourceControl(..)
    , SourceFileSpec(..)
    , PackageIDLocal
    , BinaryPackageLocal
    , SourcePackageLocal
    , PackageVersion(..)
    , PkgVersion(..)
    , prettyPkgVersion
    ) where

import Data.Text (Text)
import Debian.Arch (Arch(..))
import qualified Debian.Control.Text as T (Paragraph)
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import qualified Debian.Relation as B (PkgName, Relations)
import Debian.Release (Section(..))
import Debian.Repo.Orphans ({- instances -})
import Debian.Version (DebianVersion, prettyDebianVersion)
import System.Posix.Types (FileOffset)
import Text.PrettyPrint.ANSI.Leijen ((<>), Doc, Pretty(pretty), text)

class (Eq a, Ord a) => PackageVersion a where
    pkgName :: a -> BinPkgName
    pkgVersion :: a -> DebianVersion

-- |This is an old type which is still used to interface with the
-- Debian.Relation module.
data PkgVersion = PkgVersion { getName :: BinPkgName
                             , getVersion :: DebianVersion
                             } deriving (Eq, Ord, Show)

instance PackageVersion PkgVersion where
    pkgName = getName
    pkgVersion = getVersion

prettyPkgVersion :: PkgVersion -> Doc
prettyPkgVersion v = pretty (getName v) <> text "=" <> prettyDebianVersion (getVersion v)

---------------- PACKAGES AND PACKAGE INDEXES -------------

-- |The PackageIndex type represents a file containing control
-- information about debian packages, either source or binary.
-- Though the control information for a binary package does not
-- specify an architecture, the architecture here is that of
-- the environment where the package information is cached.
data PackageIndex
    = PackageIndex { packageIndexComponent :: Section
                   , packageIndexArch :: Arch
                   } deriving (Eq, Ord, Show)

type PackageIndexLocal = PackageIndex

prettyBinaryPackage :: BinaryPackage -> Doc
prettyBinaryPackage p = pretty (pkgName p) <> text "-" <> prettyDebianVersion (pkgVersion p)

makeBinaryPackageID :: String -> DebianVersion -> PackageID BinPkgName
makeBinaryPackageID n v = PackageID (BinPkgName n) v

makeSourcePackageID :: String -> DebianVersion -> PackageID SrcPkgName
makeSourcePackageID n v = PackageID (SrcPkgName n) v

instance PackageVersion BinaryPackage where
    pkgName = binaryPackageName
    pkgVersion = packageVersion . packageID

-- | The 'PackageID' type fully identifies a package by name, version,
-- and a 'PackageIndex' which identifies the package's release,
-- component and architecture.

data PackageID n
    = PackageID
      { packageName :: n
      , packageVersion :: DebianVersion
      } deriving (Eq, Ord, Show)

binaryPackageName :: BinaryPackage -> BinPkgName
binaryPackageName = packageName . packageID

sourcePackageName :: SourcePackage -> SrcPkgName
sourcePackageName = packageName . sourcePackageID

prettyPackageID :: B.PkgName n => PackageID n -> Doc
prettyPackageID p = pretty (packageName p) <> text "=" <> prettyDebianVersion (packageVersion p)

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
      }

instance Ord BinaryPackage where
    compare a b = compare (packageID a) (packageID b)

instance Eq BinaryPackage where
    a == b = (packageID a) == (packageID b)

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

type PackageIDLocal n = PackageID n
type BinaryPackageLocal = BinaryPackage
type SourcePackageLocal = SourcePackage
