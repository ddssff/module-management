{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Repo
    ( Repo(..)
    , RepoKey(..)
    , repoURI
    , repoKeyURI
    , repoArchList
    , libraryCompatibilityLevel
    , compatibilityFile
    ) where

import Control.Exception (throw)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.Set (Set, unions)
import Data.Text (unpack)
import Debian.Arch (Arch)
import Debian.Repo.EnvPath (EnvPath(..))
import Debian.Repo.Release (Release(releaseArchitectures))
import Debian.URI (fileFromURI, fromURI', URI')
import qualified Debian.UTF8 as Deb (decode)
import Network.URI (parseURI, URI(uriPath))
import System.FilePath ((</>))

data RepoKey
    = Remote URI'
    | Local EnvPath
      deriving (Read, Show, Eq, Ord)

class (Ord t, Eq t) => Repo t where
    repoKey :: t -> RepoKey
    repositoryCompatibilityLevel :: t -> IO (Maybe Int)
    repositoryCompatibilityLevel r =
        fileFromURI uri' >>= either throw (return . parse . unpack . Deb.decode)
        where
          uri' = uri {uriPath = uriPath uri </> compatibilityFile}
          uri = case repoKey r of
                  Remote x -> fromURI' x
                  Local x -> fromJust . parseURI $ "file://" ++ envPath x
          parse :: String -> Maybe Int
          parse s = case takeWhile isDigit s of
                         "" -> Nothing
                         s' -> Just . read $ s'
    -- | This method returns a list of all the release in the
    -- repository.  This can be used to identify all of the files
    -- in the repository that are not garbage.
    repoReleaseInfo :: t -> [Release]
    checkCompatibility :: t -> IO ()
    checkCompatibility repo =
        do level <- repositoryCompatibilityLevel repo
           case level of
             Nothing -> return ()
             Just n | n >= libraryCompatibilityLevel -> return ()
             Just n -> error ("Compatibility error: repository level " ++ show n ++
                              " < library level " ++ show libraryCompatibilityLevel ++ ", please upgrade.")

-- |The name of the file which holds the repository's compatibility
-- level.
compatibilityFile :: FilePath
compatibilityFile = "repository-compat"

-- | The compatibility level of this library and any applications
-- which use it.  It is an error if we try to use a repository whose
-- compatibility level is higher than this, a newer version of the
-- library must be used.  This value was increased from 1 to 2 due
-- to a new version number tagging policy.
libraryCompatibilityLevel :: Int
libraryCompatibilityLevel = 2

repoURI :: Repo r => r -> URI
repoURI = repoKeyURI . repoKey

repoKeyURI :: RepoKey -> URI
repoKeyURI (Local path) = fromJust . parseURI $ "file://" ++ envPath path
repoKeyURI (Remote uri) = fromURI' uri

repoArchList :: Repo r => r -> Set Arch
repoArchList = unions . map releaseArchitectures . repoReleaseInfo
