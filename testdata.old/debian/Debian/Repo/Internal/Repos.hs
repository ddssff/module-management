-- | Manage state information about the available repositories,
-- releases, OS images, and Apt images.
{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Repo.Internal.Repos
    ( MonadRepos(getRepos, putRepos)
    , ReposState
    , runReposT

    , putOSImage
    , osFromRoot

    , AptKey
    , getApt
    , getAptKey
    , putAptImage
    , evalMonadApt

    , repoByURI
    , putRepo

    , ReleaseKey
    , findRelease
    , putRelease
    , releaseByKey

    , MonadReposCached
    , runReposCachedT

    , syncOS
    ) where

import Control.Applicative ((<$>))
import Control.Applicative.Error (maybeRead)
import Control.Exception (SomeException)
import Control.Monad (unless)
import Control.Monad.Catch (bracket, catch, MonadCatch, MonadMask)
import Control.Monad.State (MonadIO(..), MonadState(get, put), StateT(runStateT))
import Control.Monad.Trans (lift)
import Data.Map as Map (empty, fromList, insert, lookup, Map, toList, union)
import Data.Maybe (fromMaybe)
import Debian.Release (ReleaseName)
import Debian.Repo.EnvPath (EnvRoot)
import Debian.Repo.Internal.Apt (AptImage(aptImageRoot))
import Debian.Repo.OSImage (OSImage(osRoot), syncOS')
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.RemoteRepository (RemoteRepository)
import Debian.Repo.Repo (Repo, repoKey, RepoKey(..))
import Debian.Repo.Top (MonadTop, runTopT, sub, TopT)
import Debian.URI (URI')
import System.IO.Error (isDoesNotExistError)
import qualified System.Posix.Files as F (removeLink)

data ReleaseKey = ReleaseKey RepoKey ReleaseName deriving (Eq, Ord, Show)

newtype AptKey = AptKey EnvRoot deriving (Eq, Ord, Show)

-- | This represents the state of the IO system.
data ReposState
    = ReposState
      { repoMap :: Map.Map URI' RemoteRepository		-- ^ Map to look up known (remote) Repository objects
      , releaseMap :: Map.Map ReleaseKey Release -- ^ Map to look up known Release objects
      , aptImageMap :: Map.Map AptKey AptImage	-- ^ Map to look up prepared AptImage objects
      , osImageMap :: Map.Map EnvRoot OSImage	-- ^ Map to look up prepared OSImage objects
      }

runReposT :: Monad m => StateT ReposState m a -> m a
runReposT action = (runStateT action) initState >>= return . fst

-- |The initial output state - at the beginning of the line, no special handle
-- state information, no repositories in the repository map.
initState :: ReposState
initState = ReposState
            { repoMap = Map.empty
            , releaseMap = Map.empty
            , aptImageMap = Map.empty
            , osImageMap = Map.empty
            }

-- | A monad to support the IO requirements of the autobuilder.
class (MonadCatch m, MonadIO m, Functor m) => MonadRepos m where
    getRepos :: m ReposState
    putRepos :: ReposState -> m ()

instance (MonadCatch m, MonadIO m, Functor m) => MonadRepos (StateT ReposState m) where
    getRepos = get
    putRepos = put

{-
instance (MonadRepos m, Functor m) => MonadRepos (StateT s m) where
    getRepos = lift getRepos
    putRepos = lift . putRepos
-}

instance MonadRepos m => MonadRepos (StateT EnvRoot m) where
    getRepos = lift getRepos
    putRepos = lift . putRepos

modifyRepos :: MonadRepos m => (ReposState -> ReposState) -> m ()
modifyRepos f = getRepos >>= putRepos . f

-- | Like @MonadRepos@, but is also an instance of MonadTop and tries to
-- load and save a list of cached repositories from @top/repoCache@.
class (MonadRepos m, MonadTop m, MonadCatch m) => MonadReposCached m

type ReposCachedT m = TopT (StateT ReposState m)

-- | To run a DebT we bracket an action with commands to load and save
-- the repository list.
runReposCachedT :: (MonadIO m, MonadCatch m, Functor m, MonadMask m) => FilePath -> ReposCachedT m a -> m a
runReposCachedT top action = do
  qPutStrLn "Running MonadReposCached..."
  r <- runReposT $ runTopT top $ bracket loadRepoCache (\ r -> saveRepoCache >> return r) (\ () -> action)
  qPutStrLn "Exited MonadReposCached..."
  return r

instance (MonadCatch m, MonadIO m, Functor m) => MonadReposCached (ReposCachedT m)

instance (MonadCatch m, MonadIO m, Functor m) => MonadRepos (ReposCachedT m) where
    getRepos = lift get
    putRepos = lift . put

putOSImage :: MonadRepos m => OSImage -> m ()
putOSImage repo =
    modifyRepos (\ s -> s {osImageMap = Map.insert (osRoot repo) repo (osImageMap s)})

osFromRoot :: MonadRepos m => EnvRoot -> m (Maybe OSImage)
osFromRoot root = Map.lookup root . osImageMap <$> getRepos

putRepo :: MonadRepos m => URI' -> RemoteRepository -> m ()
putRepo uri repo = modifyRepos (\ s -> s {repoMap = Map.insert uri repo (repoMap s)})

repoByURI :: MonadRepos m => URI' -> m (Maybe RemoteRepository)
repoByURI uri = Map.lookup uri . repoMap <$> getRepos

getApt :: MonadRepos m => EnvRoot -> m (Maybe AptImage)
getApt root = (Map.lookup (AptKey root) . aptImageMap) <$> getRepos

getAptKey :: MonadRepos m => EnvRoot -> m (Maybe AptKey)
getAptKey root = fmap (AptKey . aptImageRoot) <$> (getApt root)

findRelease :: (Repo r, MonadRepos m) => r -> ReleaseName -> m (Maybe Release)
findRelease repo dist = (Map.lookup (ReleaseKey (repoKey repo) dist) . releaseMap) <$> getRepos

releaseByKey :: MonadRepos m => ReleaseKey -> m Release
releaseByKey key = do
  Just rel <- (Map.lookup key . releaseMap) <$> getRepos
  return rel

putRelease :: (Repo r, MonadRepos m) => r -> Release -> m ReleaseKey
putRelease repo release = do
    let key = ReleaseKey (repoKey repo) (releaseName release)
    modifyRepos (\ s -> s {releaseMap = Map.insert key release (releaseMap s)})
    return key

putAptImage :: MonadRepos m => AptImage -> m AptKey
putAptImage repo = do
  let key = AptKey (aptImageRoot repo)
  modifyRepos (\ s -> s {aptImageMap = Map.insert key repo (aptImageMap s)})
  return key

-- | Run MonadOS and update the osImageMap with the modified value
evalMonadApt :: MonadRepos m => StateT AptImage m a -> AptKey -> m a
evalMonadApt task (AptKey key) = do
  Just apt <- getApt key
  (a, apt') <- runStateT task apt
  putAptImage apt'
  return a

-- | Load the value of the repo cache map from a file as a substitute for
-- downloading information from the remote repositories.  These values may
-- go out of date, as when a new release is added to a repository.  When this
-- happens some ugly errors will occur and the cache will have to be flushed.
loadRepoCache :: MonadReposCached m => m ()
loadRepoCache =
    do dir <- sub "repoCache"
       mp <- liftIO (loadRepoCache' dir `catch` (\ (e :: SomeException) -> qPutStrLn (show e) >> return Map.empty))
       modifyRepos (\ s -> s {repoMap = mp})
    where
      loadRepoCache' :: FilePath -> IO (Map URI' RemoteRepository)
      loadRepoCache' repoCache =
          do qPutStrLn "Loading repo cache..."
             file <- readFile repoCache
             case maybeRead file of
               Nothing ->
                   error ("Ignoring invalid repoCache: " ++ show file)
               Just pairs ->
                   qPutStrLn ("Loaded " ++ show (length pairs) ++ " entries from the repo cache.") >>
                   return (fromList pairs)

-- | Write the repo cache map into a file.
saveRepoCache :: MonadReposCached m => m ()
saveRepoCache =
          do path <- sub "repoCache"
             live <- repoMap <$> getRepos
             repoCache <- liftIO $ loadCache path
             let merged = Map.union live repoCache
             liftIO (F.removeLink path `catch` (\e -> unless (isDoesNotExistError e) (ioError e)) >>
                     writeFile path (show . Map.toList $ merged))
             return ()
          where
            -- isRemote uri = uriScheme uri /= "file:"
            -- isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map URI' RemoteRepository)
            loadCache path =
                readFile path `catch` (\ (_ :: SomeException) -> return "[]") >>=
                return . Map.fromList . fromMaybe [] . maybeRead

syncOS :: (MonadTop m, MonadRepos m) => OSImage -> EnvRoot -> m OSImage
syncOS srcOS dstRoot = do
  dstOS <- liftIO $ syncOS' srcOS dstRoot
  putOSImage dstOS
  dstOS' <- osFromRoot dstRoot
  maybe (error ("syncOS failed for " ++ show dstRoot)) return dstOS'
