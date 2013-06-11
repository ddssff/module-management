{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.Constants
    ( -- * Source and binary packages
      Constants(asBool, fromBool)
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where





class Constants p where
    asBool :: p -> Maybe Bool
    fromBool :: Bool -> p


