{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.SourcePackageBinaryNames
    ( -- * Source and binary packages
      sourcePackageBinaryNames
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import Debian.Repo.Package.SourceBinaryNames (sourceBinaryNames)
import Debian.Relation (BinPkgName(..))
import Debian.Repo.Types.PackageIndex (SourcePackage(sourceParagraph))

sourcePackageBinaryNames :: SourcePackage -> [BinPkgName]
sourcePackageBinaryNames package =
    sourceBinaryNames (sourceParagraph package)