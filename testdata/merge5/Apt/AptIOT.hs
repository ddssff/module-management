{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PackageImports, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Apt.AptIOT
    ( AptIOT
    ) where

import Control.Monad.State (StateT)
import Apt.AptState (AptState)

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
