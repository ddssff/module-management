{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports,
             ScopedTypeVariables, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Modules.Internal
    ( runCleanT
    , modifyParams
    , markForDelete
    , Params(..)
    , CleanT
    , MonadClean(getParams, putParams)
    , ModuleResult(..)
    , doResult
    , fixExport
    ) where

import Control.Exception (SomeException, try)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, MonadCatchIO, throw)
import Control.Monad.State (MonadState(get, put), StateT(runStateT))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Map as Map (empty, Map)
import Data.Monoid ((<>))
import Data.Sequence as Seq (Seq, (|>))
import Data.Set as Set (empty, insert, Set, toList)
import qualified Language.Haskell.Exts.Annotated as A (ExportSpec)
import Language.Haskell.Exts.Annotated.Simplify (sExportSpec)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(..), ImportDecl, ModuleName(..))
import Language.Haskell.Modules.ModuVerse (delName, ModuVerse(..), moduVerseInit, ModuVerseState, unloadModule, putModuleAnew)
import Language.Haskell.Modules.SourceDirs (modulePath, modulePathBase)
import Language.Haskell.Modules.Util.DryIO (createDirectoryIfMissing, MonadDryRun(..), removeFileIfPresent, replaceFile, tildeBackup)
import Language.Haskell.Modules.Util.QIO (MonadVerbosity(..))
import Language.Haskell.Modules.Util.Temp (withTempDirectory)
import Prelude hiding (writeFile)
import System.Directory (removeFile)
import System.FilePath (dropExtension, takeDirectory)
import System.IO.Error (isDoesNotExistError)

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

type CleanT m = StateT Params m

instance MonadClean m => ModuVerse m where
    getModuVerse = getParams >>= return . moduVerse
    modifyModuVerse f = modifyParams (\ p -> p {moduVerse = f (moduVerse p)})

class (MonadIO m, MonadCatchIO m, Functor m) => MonadClean m where
    getParams :: m Params
    putParams :: Params -> m ()

modifyParams :: MonadClean m => (Params -> Params) -> m ()
modifyParams f = getParams >>= putParams . f

instance (MonadCatchIO m, Functor m) => MonadClean (CleanT m) where
    getParams = get
    putParams = put

instance MonadClean m => MonadVerbosity m where
    getVerbosity = getParams >>= return . verbosity
    putVerbosity v = modifyParams (\ p -> p {verbosity = v})

instance MonadClean m => MonadDryRun m where
    dry = getParams >>= return . dryRun
    putDry x = modifyParams (\ p -> p {dryRun = x})

-- | Create the environment required to do import cleaning and module
-- splitting/merging.  This environment, StateT Params m a, is an
-- instance of MonadClean.
runCleanT :: MonadCatchIO m => CleanT m a -> m a
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

data ModuleResult
    = Unchanged S.ModuleName
    | Removed S.ModuleName
    | Modified S.ModuleName String
    | Created S.ModuleName String
    deriving (Show, Eq, Ord)

-- | It is tempting to put import cleaning into these operations, but
-- that needs to be done after all of these operations are completed
-- so that all the compiles required for import cleaning succeed.  On
-- the other hand, we might be able to maintain the moduVerse here.
doResult :: (ModuVerse m, MonadDryRun m, MonadVerbosity m) => ModuleResult -> m ModuleResult
doResult x@(Unchanged _name) =
    do -- quietly (qLnPutStr ("unchanged: " ++ show _name))
       return x
doResult x@(Removed name) =
    do let rel = modulePathBase "hs" name
       path <- modulePath "hs" name
       unloadModule rel
       -- I think this event handler is redundant.
       removeFileIfPresent path `IO.catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
       delName name
       return x

doResult x@(Modified name text) =
    do path <- modulePath "hs" name
       -- qLnPutStr ("modifying " ++ show path)
       -- (quietly . quietly . quietly . qPutStr $ " new text: " ++ show text)
       replaceFile tildeBackup path text
       putModuleAnew name
       return x

doResult x@(Created name text) =
    do path <- modulePath "hs" name
       -- qLnPutStr ("creating " ++ show path)
       -- (quietly . quietly . quietly . qPutStr $ " containing " ++ show text)
       createDirectoryIfMissing True (takeDirectory . dropExtension $ path)
       replaceFile tildeBackup path text
       putModuleAnew name
       return x

-- | Update an export spec.  The only thing we might need to change is
-- re-exports, of the form "module Foo".
fixExport :: [S.ModuleName] -> S.ModuleName -> S.ModuleName
          -> A.ExportSpec l -> String -> String -> String -> Seq String -> Seq String
fixExport inNames outName thisName e pref s suff r =
    case sExportSpec e of
      S.EModuleContents name
          -- when building the output module, omit re-exports of input modules
          | thisName == outName && elem name inNames -> r
          -- when building other modules, update re-exports of input
          -- modules to be a re-export of the output module.
          | elem name inNames -> r |> pref <> prettyPrint (S.EModuleContents outName) <> suff
          -- Anything else is unchanged
      _ -> r |> pref <> s <> suff
