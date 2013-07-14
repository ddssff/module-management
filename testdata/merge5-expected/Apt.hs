{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PackageImports, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Apt
    ( AptIO
    , AptIOT
    , AptState(AptState, aptImageMap, binaryPackageMap, releaseMap,
         repoMap, sourcePackageMap)
    , initState
    , MonadApt(getApt, putApt)
    ) where




import Control.Monad.State (StateT)



import qualified Data.Map as Map (Map)
import Debian.Release (ReleaseName)
import Debian.Repo.Types (AptImage, BinaryPackage, Release, SourcePackage)
import Debian.Repo.Types.Repo (RepoKey)
import Debian.Repo.Types.Repository (Repository)
import Debian.Sources (SliceName)
import System.Posix.Files (FileStatus)



import qualified Data.Map as Map (empty)

import "MonadCatchIO-mtl" Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (get, MonadIO, MonadTrans(lift), put)
import Debian.Repo.Types.Repository (MonadRepoCache(..))
import System.Posix.Files (deviceID, fileID, FileStatus, modificationTime)



import "MonadCatchIO-mtl" Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.State (MonadIO)

type AptIO = AptIOT IO


-- | A new monad to support the IO requirements of the autobuilder.
-- This uses the RWS monad.  The reader monad is used to store a flag
-- indicating whether this is a dry run, and the style information
-- associated with each output handle, including indentation, prefixing,
-- and replacing the output with one dot per n output characters.
-- The state monad stores information used to implement the current
-- output style and includes state information about whether the console
-- is at the beginning of a line, per-handle state information, and a
-- cache of the repositories that have been verified.
type AptIOT = StateT AptState


-- | This represents the state of the IO system.
data AptState
    = AptState
      { repoMap :: Map.Map RepoKey Repository		-- ^ Map to look up known Repository objects
      , releaseMap :: Map.Map (RepoKey, ReleaseName) Release -- ^ Map to look up known Release objects
      , aptImageMap :: Map.Map SliceName AptImage	-- ^ Map to look up prepared AptImage objects
      , sourcePackageMap :: Map.Map FilePath (FileStatus, [SourcePackage])
      , binaryPackageMap :: Map.Map FilePath (FileStatus, [BinaryPackage])
      }


-- |The initial output state - at the beginning of the line, no special handle
-- state information, no repositories in the repository map.
initState :: AptState
initState = AptState
            { repoMap = Map.empty
            , releaseMap = Map.empty
            , aptImageMap = Map.empty
            , sourcePackageMap = Map.empty
            , binaryPackageMap = Map.empty
            }


instance Ord FileStatus where
    compare a b = compare (deviceID a, fileID a, modificationTime a) (deviceID b, fileID b, modificationTime b)

instance Eq FileStatus where
    a == b = compare a b == EQ

instance (MonadIO m, Functor m, MonadCatchIO m) => MonadApt (AptIOT m) where
    getApt = get
    putApt = put

instance MonadApt m => MonadApt (ReaderT s m) where
    getApt = lift getApt
    putApt = lift . putApt

instance MonadApt m => MonadRepoCache m where
    getRepoCache = getApt >>= return . repoMap
    putRepoCache m = getApt >>= \ a -> putApt (a {repoMap = m})


class (MonadIO m, Functor m, MonadCatchIO m) => MonadApt m where
    getApt :: m AptState
    putApt :: AptState -> m ()

