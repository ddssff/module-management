{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.RemoteRepository
    ( RemoteRepository(..)
    ) where

import Debian.Pretty (PP(..), ppPrint)
import Debian.Repo.Release (Release)
import Debian.Repo.Repo (Repo(repoKey, repoReleaseInfo), RepoKey(Remote))
import Debian.URI (fromURI', URI')
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

data RemoteRepository
    = RemoteRepository URI' [Release]
    deriving (Read, Show, Eq, Ord)

instance Repo RemoteRepository where
    repoKey (RemoteRepository uri _) = Remote uri
    repoReleaseInfo (RemoteRepository _ info) = info

-- | URI has a bogus show function, which we are using here.
instance Pretty (PP URI') where
    pPrint = text . show . fromURI' . unPP

instance Pretty (PP RemoteRepository) where
    pPrint (PP (RemoteRepository s _)) = ppPrint s
