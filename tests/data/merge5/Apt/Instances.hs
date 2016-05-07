{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PackageImports, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Apt.Instances
    ( 
    ) where

import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (get, MonadIO, MonadTrans(lift), put)
import Apt.AptIOT (AptIOT)
import Apt.AptState (repoMap)
import Apt.MonadApt (MonadApt(getApt, putApt))
-- import Debian.Repo.State.Repository (MonadRepoCache(..))
import System.Posix.Files (deviceID, fileID, FileStatus, modificationTime)

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

{-
instance MonadApt m => MonadRepoCache m where
    getRepoCache = getApt >>= return . repoMap
    putRepoCache m = getApt >>= \ a -> putApt (a {repoMap = m})
-}
