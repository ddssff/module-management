-- | Functions to control the state variables of 'MonadClean'.
{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Modules.Params
    ( MonadClean
    , runMonadClean
    , modifyModuVerse
    , modifyRemoveEmptyImports
    , modifySourceDirs
    , modifyExtensions
    , modifyHsFlags
    , modifyDryRun
    , modifyTestMode
    ) where

import Data.Maybe (fromMaybe)
import Data.Set (empty, Set)
import Language.Haskell.Exts.Extension (Extension)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName)
import Language.Haskell.Modules.Internal (modifyParams, MonadClean, Params(dryRun, extensions, hsFlags, moduVerse, removeEmptyImports, sourceDirs, testMode), runMonadClean)
import Prelude hiding (writeFile)

-- | Modify the set of modules whose imports will be updated when
-- modules are split or merged.  No default, it is an error to run
-- splitModules or catModules without first setting this.
modifyModuVerse :: MonadClean m => (Set S.ModuleName -> Set S.ModuleName) -> m ()
modifyModuVerse f = modifyParams (\ p -> p {moduVerse = Just (f (fromMaybe empty (moduVerse p)))})

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

-- | Modify the list of directories that will be searched for source
-- files, in a similar way to the Hs-Source-Dirs field in a cabal
-- file.  Default is @[\".\"]@.
modifySourceDirs :: MonadClean m => ([FilePath] -> [FilePath]) -> m ()
modifySourceDirs f = modifyParams (\ p -> p {sourceDirs = f (sourceDirs p)})

-- | Modify the extra extensions passed to the compiler and the
-- parser.  Default value is the list in
-- 'Language.Haskell.Exts.Parser.defaultParseMode'.
modifyExtensions :: MonadClean m => ([Extension] -> [Extension]) -> m ()
modifyExtensions f = modifyParams (\ p -> p {extensions = f (extensions p)})

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
