{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.BinarySourceVersion
    ( binarySourceVersion', binarySourceVersion
    ) where


import qualified Data.Text as T (Text, unpack)
import Debian.Control (ControlFunctions(asString), Paragraph')
import qualified Debian.Control.Text as B (fieldValue)
import Debian.Relation (BinPkgName(..))
import Debian.Version (DebianVersion, parseDebianVersion)
import qualified Debian.Version as V (buildDebianVersion, epoch, revision, version)
import Text.Regex (matchRegex, mkRegex)

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

binarySourceVersion' :: (ControlFunctions a) => BinPkgName -> DebianVersion -> Paragraph' a -> Maybe (String, DebianVersion)
binarySourceVersion' binaryName binaryVersion paragraph =
    case (B.fieldValue "Source" paragraph) of
      Just source' ->
          case matchRegex re (asString source') of
            Just [name, _, ""] -> Just (name, binaryVersion)
            Just [name, _, version] -> Just (name, copyEpoch binaryVersion (parseDebianVersion version))
            _ -> error "internal error"
      Nothing ->
          Just (asString (unBinPkgName binaryName), binaryVersion)
    where
      re = mkRegex "^[ ]*([^ (]*)[ ]*(\\([ ]*([^ )]*)\\))?[ ]*$"
      -- In the Packages file the version number in the Source: field has
      -- the epoch number stripped off.  I don't know why - I should search
      -- the Debian policy manual for this.  This puts it back on.
      copyEpoch src dst = V.buildDebianVersion (V.epoch src) (V.version dst) (V.revision dst)


