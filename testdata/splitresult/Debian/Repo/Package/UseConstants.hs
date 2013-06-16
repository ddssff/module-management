{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.UseConstants
    ( useConstants
    ) where

import Debian.Repo.Package.Constants (Constants(asBool))

useConstants :: Constants p => p -> Maybe Bool
useConstants x = asBool x


