-- | The 'PackageID' type fully identifies a Debian package (source or
-- binary, depending on the n type variable) by name and version.  This
-- corresponds to Cabal's Distribution.Cabal.PackageIdentifier.
{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.PackageID
    ( PackageID(PackageID, packageVersion, packageName)
    , prettyPackageID
    , makeBinaryPackageID
    , makeSourcePackageID
    ) where

import Data.Monoid ((<>))
import Debian.Pretty (ppPrint)
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import qualified Debian.Relation as B (PkgName)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Text.PrettyPrint.HughesPJClass (Doc, text)

data PackageID n
    = PackageID
      { packageName :: n
      , packageVersion :: DebianVersion
      } deriving (Eq, Ord, Show)

prettyPackageID :: B.PkgName n => PackageID n -> Doc
prettyPackageID p = ppPrint (packageName p) <> text "-" <> prettyDebianVersion (packageVersion p)

makeBinaryPackageID :: String -> DebianVersion -> PackageID BinPkgName
makeBinaryPackageID n v = PackageID (BinPkgName n) v

makeSourcePackageID :: String -> DebianVersion -> PackageID SrcPkgName
makeSourcePackageID n v = PackageID (SrcPkgName n) v
