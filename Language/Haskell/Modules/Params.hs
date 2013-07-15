-- | Functions to control the state variables of 'MonadClean'.
{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Modules.Params
    ( modifyRemoveEmptyImports
    , modifyHsFlags
    , modifyDryRun
    , modifyTestMode
    ) where

import Language.Haskell.Modules.Internal (modifyParams, MonadClean, Params(dryRun, hsFlags, removeEmptyImports, testMode))
import Prelude hiding (writeFile)

-- | If this flag is set, imports that become empty are removed.
-- Sometimes this will lead to errors, specifically when an instance
-- in the removed import that was required is no longer be available.
-- (Note that this reflects a limitation of the
-- @-ddump-minimal-imports@ option of GHC.)  If this happens this flag
-- should be set.  Note that an import that is already empty when
-- @cleanImports@ runs will never be removed, on the assumption that
-- it was placed there only to import instances.  Default is True.
modifyRemoveEmptyImports :: MonadClean m => (Bool -> Bool) -> m ()
modifyRemoveEmptyImports f = modifyParams (\ p -> p {removeEmptyImports = f (removeEmptyImports p)})

-- | Modify the list of extra flags passed to GHC.  Default is @[]@.
modifyHsFlags :: MonadClean m => ([String] -> [String]) -> m ()
modifyHsFlags f = modifyParams (\ p -> p {hsFlags = f (hsFlags p)})

-- | Controls whether file updates will actually be performed.
-- Default is False.  (I recommend running in a directory controlled
-- by a version control system so you don't have to worry about this.)
modifyDryRun :: MonadClean m => (Bool -> Bool) -> m ()
modifyDryRun f = modifyParams (\ p -> p {dryRun = f (dryRun p)})

-- | If TestMode is turned on no import cleaning will occur after a
-- split or cat.  Default is False.
modifyTestMode :: MonadClean m => (Bool -> Bool) -> m ()
modifyTestMode f = modifyParams (\ p -> p {testMode = f (testMode p)})
