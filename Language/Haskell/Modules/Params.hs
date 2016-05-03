-- | Functions to control the state variables of 'MonadClean'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Modules.Params
    ( Params, dryRun, extraImports, hsFlags, junk, moduVerse, removeEmptyImports, scratchDir, verbosity
    , CleanMode(DoClean, NoClean)
    , CleanT
    , MonadClean(getParams, putParams)
    , modifyParams
    , runImportsT
    , markForDelete
    , modifyRemoveEmptyImports
    , modifyHsFlags
    , modifyDryRun
    , extraImport
    ) where

import Control.Exception (SomeException, try)
import Control.Lens (makeLenses)
import Control.Monad.Trans.Control as IO (MonadBaseControl)
import Control.Monad.State (MonadState(get, put), StateT(runStateT))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Map as Map (empty, Map, insertWith)
import Data.Monoid ((<>))
import Data.Set as Set (empty, insert, Set, toList)
import Language.Haskell.Exts.SrcLoc (SrcLoc(SrcLoc))
import Language.Haskell.Exts.Syntax as S (ImportDecl(..), ModuleName)
import Language.Haskell.Modules.ModuVerse (ModuVerse(..), moduVerseInit, ModuVerseState)
import Language.Haskell.Modules.Util.DryIO (MonadDryRun(..))
import Language.Haskell.Modules.Util.QIO (MonadVerbosity(..))
import Language.Haskell.Modules.Util.Temp (withTempDirectory)
import Prelude hiding (writeFile, writeFile)
import System.Directory (removeFile)

data CleanMode = DoClean | NoClean deriving (Eq, Ord, Show)

-- | This contains the information required to run the state monad for
-- import cleaning and module spliting/mergeing.
data Params
    = Params
      { _scratchDir :: FilePath
      -- ^ Location of the temporary directory for ghc output.
      , _dryRun :: Bool
      -- ^ None of the operations that modify the modules will actually
      -- be performed if this is ture.
      , _verbosity :: Int
      -- ^ Increase or decrease the amount of progress reporting.
      , _hsFlags :: [String]
      -- ^ Extra flags to pass to GHC.
      , _moduVerse :: ModuVerseState
      -- ^ The set of modules that splitModules and catModules will
      -- check for imports of symbols that moved.
      , _junk :: Set FilePath
      -- ^ Paths added to this list are removed as the state monad
      -- finishes.
      , _removeEmptyImports :: Bool
      -- ^ If true, remove any import that became empty due to the
      -- clean.  THe import might still be required because of the
      -- instances it contains, but usually it is not.  Note that this
      -- option does not affect imports that started empty and end
      -- empty.
      , _extraImports :: Map S.ModuleName [S.ImportDecl]
      -- ^ Deciding whether a module needs to be imported can be
      -- difficult when instances are involved, this is a cheat to force
      -- keys of the map to import the corresponding elements.
      -- , testMode :: CleanMode
      -- ^ For testing, do not run cleanImports on the results of the
      -- splitModule and catModules operations.
      } deriving (Eq, Ord, Show)

$(makeLenses ''Params)

-- | An instance of MonadClean.
type CleanT m = StateT Params m

instance MonadClean m => ModuVerse m where
    getModuVerse = getParams >>= return . _moduVerse
    modifyModuVerse f = modifyParams (\ p -> p {_moduVerse = f (_moduVerse p)})

class (MonadIO m, MonadBaseControl IO m, Functor m, MonadState Params m) => MonadClean m where
    getParams :: m Params
    putParams :: Params -> m ()

modifyParams :: MonadClean m => (Params -> Params) -> m ()
modifyParams f = getParams >>= putParams . f

instance (MonadIO m, MonadBaseControl IO m, Functor m) => MonadClean (CleanT m) where
    getParams = get
    putParams = put

instance MonadClean m => MonadVerbosity m where
    getVerbosity = getParams >>= return . _verbosity
    putVerbosity v = modifyParams (\ p -> p {_verbosity = v})

instance MonadClean m => MonadDryRun m where
    dry = getParams >>= return . _dryRun
    putDry x = modifyParams (\ p -> p {_dryRun = x})

-- | Create the environment required to do import cleaning and module
-- splitting/merging.  This environment, @StateT Params m a@, is an
-- instance of 'MonadClean'.
runImportsT :: (MonadIO m, MonadBaseControl IO m) => CleanT m a -> m a
runImportsT action =
    withTempDirectory "." "hmm" $ \ scratch ->
    do (result, params) <- runStateT action (Params {_scratchDir = scratch,
                                                     _dryRun = False,
                                                     _verbosity = 1,
                                                     _hsFlags = [],
                                                     _moduVerse = moduVerseInit,
                                                     _junk = Set.empty,
                                                     _removeEmptyImports = True,
                                                     _extraImports = Map.empty})
       mapM_ (\ x -> liftIO (try (removeFile x)) >>= \ (_ :: Either SomeException ()) -> return ()) (toList (_junk params))
       return result

markForDelete :: MonadClean m => FilePath -> m ()
markForDelete x = modifyParams (\ p -> p {_junk = insert x (_junk p)})

-- | If this flag is set, imports that become empty are removed.
-- Sometimes this will lead to errors, specifically when an instance
-- in the removed import that was required is no longer be available.
-- (Note that this reflects a limitation of the
-- @-ddump-minimal-imports@ option of GHC.)  If this happens this flag
-- should be set.  Note that an import that is already empty when
-- @cleanImports@ runs will never be removed, on the assumption that
-- it was placed there only to import instances.  Default is True.
modifyRemoveEmptyImports :: MonadClean m => (Bool -> Bool) -> m ()
modifyRemoveEmptyImports f = modifyParams (\ p -> p {_removeEmptyImports = f (_removeEmptyImports p)})

-- | Modify the list of extra flags passed to GHC.  Default is @[]@.
modifyHsFlags :: MonadClean m => ([String] -> [String]) -> m ()
modifyHsFlags f = modifyParams (\ p -> p {_hsFlags = f (_hsFlags p)})

-- | Controls whether file updates will actually be performed.
-- Default is False.  (I recommend running in a directory controlled
-- by a version control system so you don't have to worry about this.)
modifyDryRun :: MonadClean m => (Bool -> Bool) -> m ()
modifyDryRun f = modifyParams (\ p -> p {_dryRun = f (_dryRun p)})

-- | When we write module @m@, insert an extra line that imports the
-- instances (only) from module @i@.
extraImport :: MonadClean m => S.ModuleName -> S.ModuleName -> m ()
extraImport m i =
    modifyParams (\ p -> p {_extraImports = Map.insertWith (<>) m [im] (_extraImports p)})
    where im = ImportDecl { importLoc = SrcLoc "<unknown>.hs" 1 1
                          , importModule = i
                          , importQualified = False
                          , importSrc = False
                          , importSafe = False
                          , importPkg = Nothing
                          , importAs = Nothing
                          , importSpecs = Just (False, []) }
