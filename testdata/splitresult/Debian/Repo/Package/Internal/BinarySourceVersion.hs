{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.Internal.BinarySourceVersion
    ( binarySourceVersion'
    ) where


import Debian.Control (ControlFunctions(asString), Paragraph')
import qualified Debian.Control.Text as B (fieldValue)
import Debian.Relation (BinPkgName(..))
import Debian.Version (DebianVersion, parseDebianVersion)
import qualified Debian.Version as V (buildDebianVersion, epoch, revision, version)
import Text.Regex (matchRegex, mkRegex)

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

