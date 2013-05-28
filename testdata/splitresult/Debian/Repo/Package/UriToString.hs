{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing  #-}
module Debian.Repo.Package.UriToString (uriToString') where

import Network.URI (URI, uriToString)

uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""

