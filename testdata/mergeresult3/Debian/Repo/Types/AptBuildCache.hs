{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.AptBuildCache
    ( AptBuildCache(..)
    ) where

import Debian.Repo.Types.AptCache (AptCache)
import Debian.Repo.Types.Slice (SliceList)

-- | An apt cache with extra sources.list lines for a local upload repository.
class AptCache t => AptBuildCache t where
    -- | The sources.list
    aptSliceList :: t -> SliceList
