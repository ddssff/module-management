{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Slice
    ( EnvRoot(..), EnvPath(..), outsidePath, appendPath, rootEnvPath, Repo(..), RepoKey(..), repoURI, repoKeyURI, libraryCompatibilityLevel, compatibilityFile, Slice(..), SliceList(..), NamedSliceList(..)
    ) where

import Control.Exception (throw)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Debian.Repo.Types.Release (Release)
import Debian.Sources (DebSource(..), SliceName(..), SourceType(..))
import Debian.URI (fileFromURI, fromURI', URI')
import qualified Debian.UTF8 as Deb (decode)
import Network.URI (parseURI, URI(uriPath))
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), vcat)

 
data Slice = Slice{sliceRepoKey :: Debian.Repo.Types.Slice.RepoKey,
                   sliceSource :: DebSource}
           deriving (Eq, Ord, Show)

-- | Each line of the sources.list represents a slice of a repository
data SliceList = SliceList {slices :: [Slice]} deriving (Eq, Ord, Show)

data NamedSliceList
    = NamedSliceList { sliceList :: SliceList
                     , sliceListName :: SliceName
                     } deriving (Eq, Ord, Show)

instance Pretty SliceList where
    pretty = vcat . map (pretty . sliceSource) . slices

deriving instance Show SourceType
deriving instance Show DebSource



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




-- |The root directory of an OS image.
data EnvRoot = EnvRoot { rootPath :: FilePath } deriving (Ord, Eq, Read, Show)

-- |A directory inside of an OS image.
data EnvPath = EnvPath { envRoot :: EnvRoot
                       , envPath :: FilePath
                       } deriving (Ord, Eq, Read, Show)

outsidePath :: EnvPath -> FilePath
outsidePath path = rootPath (envRoot path) ++ envPath path

appendPath :: FilePath -> EnvPath -> EnvPath
appendPath suff path = path { envPath = envPath path ++ suff }

rootEnvPath :: FilePath -> EnvPath
rootEnvPath s = EnvPath { envRoot = EnvRoot "", envPath = s }


