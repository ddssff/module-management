-- | A repository located on localhost
{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, StandaloneDeriving, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.State.Repository
    ( readLocalRepository
    , prepareLocalRepository
    , prepareLocalRepository'
    , prepareRemoteRepository
    , repairLocalRepository
    , foldRepository
    ) where

import Control.Monad (filterM, when)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Maybe (catMaybes)
import Debian.Release (ReleaseName(ReleaseName), Section(Section))
import Debian.Repo.EnvPath (EnvPath, EnvPath(EnvPath), EnvRoot(EnvRoot), outsidePath)
import Debian.Repo.Internal.Repos (MonadRepos(..), repoByURI, putRepo)
import Debian.Repo.LocalRepository (Layout(..), LocalRepository(..), readLocalRepo)
import Debian.Repo.Release (getReleaseInfoRemote, parseArchitectures, Release(Release, releaseAliases, releaseArchitectures, releaseComponents, releaseName))
import Debian.Repo.RemoteRepository (RemoteRepository, RemoteRepository(RemoteRepository))
import Debian.Repo.Repo (RepoKey(..))
import Debian.URI (fromURI', toURI', URI(uriPath), URI')
import Network.URI (URI(..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Posix.Files as F (fileMode, getFileStatus, setFileMode)
import Text.Regex (matchRegex, mkRegex)

repairLocalRepository :: MonadIO m => LocalRepository -> m LocalRepository
repairLocalRepository r = prepareLocalRepository (repoRoot r) (repoLayout r) (repoReleaseInfoLocal r)

createLocalRepository :: MonadIO m => EnvPath -> Maybe Layout -> m (Maybe Layout)
createLocalRepository root layout = do
  mapM_ (liftIO . initDir)
            [(".", 0o40755),
             ("dists", 0o40755),
             ("incoming", 0o41755),
             ("removed", 0o40750),
             ("reject", 0o40750)]
  -- If repo exists compute its actual layout
  layout' <- liftIO (computeLayout (outsidePath root)) >>= return . maybe layout Just
  -- >>= return . maybe (maybe (error "No layout specified for new repository") id layout) id
  mapM_ (liftIO . initDir)
            (case layout' of
               Just Pool -> [("pool", 0o40755), ("installed", 0o40755)]
               Just Flat -> []
               Nothing -> [])
  return layout'
    where
      initDir (name, mode) =
          do let path = outsidePath root </> name
             filterM (\ f -> doesDirectoryExist f >>= return . not) [path] >>=
                     mapM_ (\ f -> createDirectoryIfMissing True f)
             actualMode <- F.getFileStatus path >>= return . F.fileMode
             when (mode /= actualMode) (F.setFileMode path mode)

readLocalRepository :: MonadIO m => EnvPath -> Maybe Layout -> m (Maybe LocalRepository)
readLocalRepository root layout = createLocalRepository root layout >>= readLocalRepo root

-- | Create or verify the existance of the directories which will hold
-- a repository on the local machine.  Verify the index files for each of
-- its existing releases.
prepareLocalRepository :: MonadIO m => EnvPath -> Maybe Layout -> [Release] -> m LocalRepository
prepareLocalRepository root layout releases =
    readLocalRepository root layout >>= maybe (return $ makeLocalRepo root layout releases) return

prepareLocalRepository' :: MonadIO m => EnvPath -> Maybe Layout -> m LocalRepository
prepareLocalRepository' root layout =
    prepareLocalRepository root layout [Release { releaseName = ReleaseName "precise-seereason"
                                                , releaseAliases = []
                                                , releaseArchitectures = parseArchitectures "amd64, i386"
                                                , releaseComponents = [Section "main"] }]

makeLocalRepo :: EnvPath -> Maybe Layout -> [Release] -> LocalRepository
makeLocalRepo root layout releases =
    LocalRepository { repoRoot = root
                    , repoLayout = layout
                    , repoReleaseInfoLocal = releases }

-- |Try to determine a repository's layout.
computeLayout :: FilePath -> IO (Maybe Layout)
computeLayout root =
    do
      -- If there are already .dsc files in the root directory
      -- the repository layout is Flat.
      isFlat <- getDirectoryContents root >>= return . (/= []) . catMaybes . map (matchRegex (mkRegex "\\.dsc$"))
      -- If the pool directory already exists the repository layout is
      -- Pool.
      isPool <- doesDirectoryExist (root ++ "/pool")
      case (isFlat, isPool) of
        (True, _) -> return (Just Flat)
        (False, True) -> return (Just Pool)
        _ -> return Nothing

prepareRemoteRepository :: MonadRepos m => URI -> m RemoteRepository
prepareRemoteRepository uri =
    let uri' = toURI' uri in
    repoByURI uri' >>= maybe (loadRemoteRepository uri') return

-- |To create a RemoteRepo we must query it to find out the
-- names, sections, and supported architectures of its releases.
loadRemoteRepository :: MonadRepos m => URI' -> m RemoteRepository
loadRemoteRepository uri =
    do releaseInfo <- liftIO . unsafeInterleaveIO . getReleaseInfoRemote . fromURI' $ uri
       let repo = RemoteRepository uri releaseInfo
       putRepo uri repo
       return repo

-- foldRepository :: forall m r a. MonadState ReposState m => (r -> m a) -> RepoKey -> m a
-- foldRepository f key =
--     case key of
--       Local path -> prepareLocalRepository path Nothing >>= f
--       Remote uri' ->
--           let uri = fromURI' uri' in
--           case uriScheme uri of
--             "file:" -> prepareLocalRepository (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= f
--             _ -> prepareRemoteRepository uri >>= f

foldRepository :: MonadRepos m => (LocalRepository -> m a) -> (RemoteRepository -> m a) -> RepoKey -> m a
foldRepository f g key =
    case key of
      Local path -> readLocalRepository path Nothing >>= maybe (error $ "No repository at " ++ show path) f
      Remote uri' ->
          let uri = fromURI' uri' in
          case uriScheme uri of
            "file:" -> prepareLocalRepository' (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= f
            _ -> prepareRemoteRepository uri >>= g
