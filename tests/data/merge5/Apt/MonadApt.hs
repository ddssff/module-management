{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PackageImports, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Apt.MonadApt
    ( MonadApt(getApt, putApt)
    ) where

import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.State (MonadIO)
import Apt.AptState (AptState)

class (MonadIO m, Functor m, MonadCatchIO m) => MonadApt m where
    getApt :: m AptState
    putApt :: AptState -> m ()
