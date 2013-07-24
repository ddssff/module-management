-- | Functions to control the state variables of 'MonadClean'.
{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Modules.Params
    ( Params(Params, dryRun, extraImports, hsFlags, junk, moduVerse,
       removeEmptyImports, scratchDir, testMode, verbosity)
    , CleanT
    , MonadClean(getParams, putParams)
    , modifyParams
    , runCleanT
    , markForDelete
    , modifyRemoveEmptyImports
    , modifyHsFlags
    , modifyDryRun
    , modifyTestMode
    , extraImport
    ) where

import Control.Exception (SomeException, try)
import Control.Monad.Trans.Control as IO (MonadBaseControl)
import Control.Monad.State (MonadState(get, put), StateT(runStateT))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Map as Map (empty, Map, insertWith)
import Data.Set as Set (empty, insert, Set, singleton, toList, union)
import Language.Haskell.Exts.SrcLoc (SrcLoc(SrcLoc))
import Language.Haskell.Exts.Syntax as S (ImportDecl(..), ModuleName)
import Language.Haskell.Modules.ModuVerse (ModuVerse(..), moduVerseInit, ModuVerseState)
import Language.Haskell.Modules.Util.DryIO (MonadDryRun(..))
import Language.Haskell.Modules.Util.QIO (MonadVerbosity(..))
import Language.Haskell.Modules.Util.Temp (withTempDirectory)
import Prelude hiding (writeFile, writeFile)
import System.Directory (removeFile)

-- | This contains the information required to run the state monad for
-- import cleaning and module spliting/mergeing.
data Params
    = Params
      { scratchDir :: FilePath
      -- ^ Location of the temporary directory for ghc output.
      , dryRun :: Bool
      -- ^ None of the operations that modify the modules will actually
      -- be performed if this is ture.
      , verbosity :: Int
      -- ^ Increase or decrease the amount of progress reporting.
      , hsFlags :: [String]
      -- ^ Extra flags to pass to GHC.
      , moduVerse :: ModuVerseState
      -- ^ The set of modules that splitModules and catModules will
      -- check for imports of symbols that moved.
      , junk :: Set FilePath
      -- ^ Paths added to this list are removed as the state monad
      -- finishes.
      , removeEmptyImports :: Bool
      -- ^ If true, remove any import that became empty due to the
      -- clean.  THe import might still be required because of the
      -- instances it contains, but usually it is not.  Note that this
      -- option does not affect imports that started empty and end
      -- empty.
      , extraImports :: Map S.ModuleName (Set S.ImportDecl)
      -- ^ Deciding whether a module needs to be imported can be
      -- difficult when instances are involved, this is a cheat to force
      -- keys of the map to import the corresponding elements.
      , testMode :: Bool
      -- ^ For testing, do not run cleanImports on the results of the
      -- splitModule and catModules operations.
      } deriving (Eq, Ord, Show)

-- | An instance of MonadClean.
type CleanT m = StateT Params m

instance MonadClean m => ModuVerse m where
    getModuVerse = getParams >>= return . moduVerse
    modifyModuVerse f = modifyParams (\ p -> p {moduVerse = f (moduVerse p)})

class (MonadIO m, MonadBaseControl IO m, Functor m) => MonadClean m where
    getParams :: m Params
    putParams :: Params -> m ()

modifyParams :: MonadClean m => (Params -> Params) -> m ()
modifyParams f = getParams >>= putParams . f

instance (MonadIO m, MonadBaseControl IO m, Functor m) => MonadClean (CleanT m) where
    getParams = get
    putParams = put

instance MonadClean m => MonadVerbosity m where
    getVerbosity = getParams >>= return . verbosity
    putVerbosity v = modifyParams (\ p -> p {verbosity = v})

instance MonadClean m => MonadDryRun m where
    dry = getParams >>= return . dryRun
    putDry x = modifyParams (\ p -> p {dryRun = x})

-- | Create the environment required to do import cleaning and module
-- splitting/merging.  This environment, @StateT Params m a@, is an
-- instance of 'MonadClean'.
runCleanT :: (MonadIO m, MonadBaseControl IO m) => CleanT m a -> m a
runCleanT action =
    withTempDirectory "." "scratch" $ \ scratch ->
    do (result, params) <- runStateT action (Params {scratchDir = scratch,
                                                     dryRun = False,
                                                     verbosity = 1,
                                                     hsFlags = [],
                                                     moduVerse = moduVerseInit,
                                                     junk = Set.empty,
                                                     removeEmptyImports = True,
                                                     extraImports = Map.empty,
                                                     testMode = False})
       mapM_ (\ x -> liftIO (try (removeFile x)) >>= \ (_ :: Either SomeException ()) -> return ()) (toList (junk params))
       return result

markForDelete :: MonadClean m => FilePath -> m ()
markForDelete x = modifyParams (\ p -> p {junk = insert x (junk p)})

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
-- split or cat.  Default is False.  Note that the modules produced
-- with this option will often fail to compile to to circular imports.
-- (Does this seem counterintuitive to anyone else?)
modifyTestMode :: MonadClean m => (Bool -> Bool) -> m ()
modifyTestMode f = modifyParams (\ p -> p {testMode = f (testMode p)})

-- | When we write module @m@, insert an extra line that imports the
-- instances (only) from module @i@.
extraImport :: MonadClean m => S.ModuleName -> S.ModuleName -> m ()
extraImport m i =
    modifyParams (\ p -> p {extraImports = Map.insertWith (union) m (singleton im) (extraImports p)})
    where im = ImportDecl { importLoc = SrcLoc "<unknown>.hs" 1 1
                          , importModule = i
                          , importQualified = False
                          , importSrc = False
                          , importPkg = Nothing
                          , importAs = Nothing
                          , importSpecs = Just (False, []) }
