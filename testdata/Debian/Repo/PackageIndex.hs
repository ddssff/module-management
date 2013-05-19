module Debian.Repo.PackageIndex
    ( packageIndexName
    , packageIndexPath
    , packageIndexDir
    , packageIndexPathList
    , packageIndexDirList
    , packageIndexList
    , sourceIndexList
    , binaryIndexList
    , releaseDir
    , showIndexBrief
    , debSourceFromIndex
    ) where

import Debian.Arch (Arch(..), prettyArch)
import Debian.Release (releaseName', sectionName')
import Debian.Repo.Types (PackageIndex(..), Release(..))
import Debian.Repo.Types.Repo (repoURI)
import Debian.Repo.Types.Repository (Repository)
import Debian.Sources (DebSource(..), SourceType(..))
import System.FilePath ((</>))

packageIndexName :: PackageIndex -> FilePath
packageIndexName index =
    case packageIndexArch index of
      Source -> "Sources"
      _ -> "Packages"

packageIndexPath :: Release -> PackageIndex -> FilePath
packageIndexPath release index = packageIndexDir release index ++ "/" ++ packageIndexName index

packageIndexDir :: Release -> PackageIndex -> FilePath
packageIndexDir release index =
    case packageIndexArch index of
      Source -> releaseDir release ++ "/" ++ sectionName' (packageIndexComponent index) ++ "/source"
      _ -> (releaseDir release ++ "/" ++
            sectionName' (packageIndexComponent index) ++
            -- Will prettyArch give us linux-amd64 when we just want amd64?
            "/binary-" ++ show (prettyArch (packageIndexArch index)))

releaseDir :: Release -> String
releaseDir release = "dists/" ++ (releaseName' . releaseName $ release)

packageIndexPathList :: Release -> [FilePath]
packageIndexPathList release = map (packageIndexPath release) . packageIndexList $ release

packageIndexDirList :: Release -> [FilePath]
packageIndexDirList release = map (packageIndexDir release) . packageIndexList $ release

packageIndexList :: Release -> [PackageIndex]
packageIndexList release = sourceIndexList release ++ binaryIndexList release

sourceIndexList :: Release -> [PackageIndex]
sourceIndexList release =
    map componentIndex (releaseComponents $ release)
    where componentIndex component = PackageIndex { packageIndexComponent = component
                                                  , packageIndexArch = Source }

binaryIndexList :: Release -> [PackageIndex]
binaryIndexList release =
    concat . map componentIndexes $ (releaseComponents release)
    where
      --componentIndexes :: Section -> [PackageIndex]
      componentIndexes component =
          map archIndex (filter (/= Source) (releaseArchitectures release))
          where
            --archIndex :: Arch -> PackageIndex
            archIndex arch = PackageIndex { packageIndexComponent = component
                                          , packageIndexArch = arch }

showIndexBrief :: (Repository, Release) -> PackageIndex -> String
showIndexBrief release index =
    (releaseName' . releaseName . snd $ release) </> sectionName' (packageIndexComponent index) </> showArch (packageIndexArch index)
    where showArch Source = "source"
          showArch All = "all"
          showArch x@(Binary _ _) = "binary-" ++ show (prettyArch x)

debSourceFromIndex :: (Repository, Release) -> PackageIndex -> DebSource
debSourceFromIndex (repo, release) index =
    DebSource {sourceType = typ,
               sourceUri = repoURI repo,
               sourceDist = Right (dist, components)}
    where
      typ = case arch of (Binary _ _) -> Deb; Source -> DebSrc; All -> Deb
      arch = packageIndexArch index
      dist = releaseName $ release
      components = releaseComponents $ release
