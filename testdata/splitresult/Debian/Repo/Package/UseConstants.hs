{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.UseConstants
    ( -- * Source and binary packages
      useConstants
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import Debian.Repo.Package.Constants (Constants(asBool))

useConstants :: Constants p => p -> Maybe Bool
useConstants x = asBool x
