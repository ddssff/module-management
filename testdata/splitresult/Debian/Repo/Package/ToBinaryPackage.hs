{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.ToBinaryPackage
    ( -- * Source and binary packages
      toBinaryPackage
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import qualified Data.Text as T (unpack)
import qualified Debian.Control.Text as B (ControlFunctions(lookupP), fieldValue, Paragraph)
import Debian.Repo.Package.TryParseRel (tryParseRel)
import Debian.Repo.PackageIndex (packageIndexPath)
import Debian.Repo.Types.PackageIndex (BinaryPackage(..), makeBinaryPackageID, PackageIndex)
import Debian.Repo.Types.Release (Release)
import Debian.Version (parseDebianVersion)

toBinaryPackage :: Release -> PackageIndex -> B.Paragraph -> BinaryPackage
toBinaryPackage release index p =
    case (B.fieldValue "Package" p, B.fieldValue "Version" p) of
      (Just name, Just version) ->
          BinaryPackage
          { packageID =
                makeBinaryPackageID (T.unpack name) (parseDebianVersion (T.unpack version))
          , packageInfo = p
          , pDepends = tryParseRel $ B.lookupP "Depends" p
          , pPreDepends = tryParseRel $ B.lookupP "Pre-Depends" p
          , pConflicts = tryParseRel $ B.lookupP "Conflicts" p
          , pReplaces =  tryParseRel $ B.lookupP "Replaces" p
          , pProvides =  tryParseRel $ B.lookupP "Provides" p
          }
      _ -> error ("Invalid data in source index:\n " ++ packageIndexPath release index)


