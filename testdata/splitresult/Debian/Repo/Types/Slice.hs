{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Slice
    ( Slice(..)
    , SliceList(..)
    , NamedSliceList(..)
    ) where

import Debian.Repo.Types.Repo (RepoKey)
import Debian.Sources (DebSource(..), SliceName(..), SourceType(..))
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), vcat)

data Slice = Slice {sliceRepoKey :: RepoKey, sliceSource :: DebSource} deriving (Eq, Ord, Show)

-- | Each line of the sources.list represents a slice of a repository
data SliceList = SliceList {slices :: [Slice]} deriving (Eq, Ord, Show)

data NamedSliceList
    = NamedSliceList { sliceList :: SliceList
                     , sliceListName :: SliceName
                     } deriving (Eq, Ord, Show)

instance Pretty SliceList where
    pretty = vcat . map (pretty . sliceSource) . slices

deriving instance Show SourceType
deriving instance Show DebSource
