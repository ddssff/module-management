{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.SourceFilePaths
    ( sourceFilePaths
    ) where


import Data.Set as Set (fromList, map, Set)
import Debian.Repo.Types.PackageIndex (SourceFileSpec(sourceFileName), SourcePackage(sourceDirectory, sourcePackageFiles))
import System.FilePath ((</>))

sourceFilePaths :: SourcePackage -> Set FilePath
sourceFilePaths package =
    Set.map ((sourceDirectory package) </>) . Set.map sourceFileName . Set.fromList . sourcePackageFiles $ package

