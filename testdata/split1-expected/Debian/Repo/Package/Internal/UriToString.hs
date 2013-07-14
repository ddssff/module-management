{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.Internal.UriToString
    ( uriToString'
    ) where

import Network.URI (URI, uriToString)

uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""
