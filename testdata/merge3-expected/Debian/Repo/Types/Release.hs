{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Release
    ( Release(Release, releaseName, releaseAliases, releaseArchitectures, releaseComponents)
    , makeReleaseInfo
    , parseComponents
    , parseArchitectures
    ) where

import Control.Applicative.Error (Failing(Success, Failure))
import Data.Text (Text, unpack)
import Debian.Arch (Arch(..), parseArch)
import qualified Debian.Control.Text as T (fieldValue, Paragraph)
import Debian.Release (parseSection', ReleaseName(..), Section(..))
import Text.Regex (mkRegex, splitRegex)
import qualified Tmp.File as F (File(..))

-- FIXME: The lists here should be sets so that == and compare work properly.
data Release = Release { releaseName :: ReleaseName
                       , releaseAliases :: [ReleaseName]
                       , releaseArchitectures :: [Arch]
                       , releaseComponents :: [Section]
                       } deriving (Eq, Ord, Read, Show)

makeReleaseInfo :: F.File T.Paragraph -> ReleaseName -> [ReleaseName] -> Release
makeReleaseInfo file@(F.File {F.text = Failure msgs}) _name _aliases =
    error $ "Failure reading " ++ show (F.path file) ++ ": " ++ show msgs
makeReleaseInfo file@(F.File {F.text = Success info}) name aliases =
    case (T.fieldValue "Architectures" info, T.fieldValue "Components" info) of
      (Just archList, Just compList) ->
          Release { releaseName = name
                  , releaseAliases = aliases
                  , releaseArchitectures = parseArchitectures archList
                  , releaseComponents = parseComponents compList }
      _ -> error $ "Missing Architectures or Components field in Release file " ++ show (F.path file)

parseArchitectures :: Text -> [Arch]
parseArchitectures archList =
    map parseArch . splitRegex re . unpack $ archList
    where
      re = mkRegex "[ ,]+"

parseComponents :: Text -> [Section]
parseComponents compList =
    map parseSection' . splitRegex re . unpack  $ compList
    where
      re = mkRegex "[ ,]+"
