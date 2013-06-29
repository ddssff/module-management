{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.Internal.UseConstants
    ( useConstants
    ) where

import Debian.Repo.Package.Internal.Constants (Constants(asBool))

useConstants :: Constants p => p -> Maybe Bool
useConstants x = asBool x

