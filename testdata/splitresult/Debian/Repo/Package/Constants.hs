{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.Constants
    ( Constants(asBool, fromBool)
    ) where




class Constants p where
    asBool :: p -> Maybe Bool
    fromBool :: Bool -> p


