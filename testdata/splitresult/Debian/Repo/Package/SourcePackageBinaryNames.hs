{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.SourcePackageBinaryNames
    ( sourcePackageBinaryNames
    ) where

import Debian.Relation (BinPkgName(..))
import Debian.Repo.Package.SourceBinaryNames (sourceBinaryNames)
import Debian.Repo.Types.PackageIndex (SourcePackage(sourceParagraph))

sourcePackageBinaryNames :: SourcePackage -> [BinPkgName]
sourcePackageBinaryNames package =
    sourceBinaryNames (sourceParagraph package)
