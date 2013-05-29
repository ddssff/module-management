{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.UriToString
    ( -- * Source and binary packages
      uriToString'
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import Network.URI (URI, uriToString)



uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""
