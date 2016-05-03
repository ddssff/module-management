{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.BinarySourceVersion
    ( binarySourceVersion
    ) where

import qualified Data.Text as T (Text, unpack)
import Debian.Control (Paragraph')
import qualified Debian.Control.Text as B (fieldValue)
import Debian.Relation (BinPkgName(..))
import Debian.Repo.Package.Internal.BinarySourceVersion (binarySourceVersion')
import Debian.Version (DebianVersion, parseDebianVersion)

-- |Return the name and version number of the source package that
-- generated this binary package.
-- see also: 'binaryPackageSourceVersion'
binarySourceVersion :: Paragraph' T.Text -> Maybe ((BinPkgName, DebianVersion), (String, DebianVersion))
binarySourceVersion paragraph =
    let mBinaryName = fmap (BinPkgName . T.unpack) $ B.fieldValue "Package" paragraph
        mBinaryVersion = fmap (parseDebianVersion . T.unpack) $ B.fieldValue "Version" paragraph
    in
      case (mBinaryName, mBinaryVersion) of
        (Just binaryName, Just binaryVersion) ->
            fmap ((,) (binaryName, binaryVersion)) $ binarySourceVersion' binaryName binaryVersion paragraph
        _ -> Nothing
