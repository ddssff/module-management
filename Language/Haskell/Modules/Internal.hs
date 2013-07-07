{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports,
             ScopedTypeVariables, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Modules.Internal
    ( runMonadClean
    , modifyParams
    -- , parseFileWithComments
    -- , parseFile
    , markForDelete
    , Params(..)
    , MonadClean(getParams, putParams)
    , ModuleResult(..)
    , doResult
    ) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, MonadCatchIO, throw)
import Control.Monad.State (MonadState(get, put), StateT(runStateT))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Set as Set (empty, insert, Set, toList)
--import qualified Language.Haskell.Exts.Annotated as A (Module(..), ModuleHead(..), parseFileWithComments)
--import Language.Haskell.Exts.Annotated.Simplify (sModuleName)
--import Language.Haskell.Exts.Comments (Comment(..))
--import Language.Haskell.Exts.Extension (Extension)
--import qualified Language.Haskell.Exts.Parser as Exts (defaultParseMode, ParseMode(extensions, parseFilename), ParseResult, fromParseResult)
--import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..))
import Language.Haskell.Modules.ModuVerse (ModuleInfo, ModuVerse(..), ModuVerseState, moduVerseInit,
                                           putName, delName, modulePath, parseModule)
import Language.Haskell.Modules.Util.DryIO (createDirectoryIfMissing, MonadDryRun(..), removeFileIfPresent, replaceFile, tildeBackup)
import Language.Haskell.Modules.Util.QIO (MonadVerbosity(..), qLnPutStr, qPutStr, quietly)
import Language.Haskell.Modules.Util.Temp (withTempDirectory)
import Prelude hiding (writeFile)
import System.Directory (doesFileExist, getCurrentDirectory, removeFile)
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
      , testMode :: Bool
      -- ^ For testing, do not run cleanImports on the results of the
      -- splitModule and catModules operations.
      } deriving (Eq, Ord, Show)

instance MonadClean m => ModuVerse m where
    getModuVerse = getParams >>= return . moduVerse
    modifyModuVerse f = modifyParams (\ p -> p {moduVerse = f (moduVerse p)})

class (MonadIO m, MonadCatchIO m, Functor m) => MonadClean m where
    getParams :: m Params
    putParams :: Params -> m ()

modifyParams :: MonadClean m => (Params -> Params) -> m ()
modifyParams f = getParams >>= putParams . f

instance (MonadCatchIO m, Functor m) => MonadClean (StateT Params m) where
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
runMonadClean :: MonadCatchIO m => StateT Params m a -> m a
runMonadClean action =
    withTempDirectory "." "scratch" $ \ scratch ->
    do (result, params) <- runStateT action (Params {scratchDir = scratch,
                                                     dryRun = False,
                                                     verbosity = 1,
                                                     hsFlags = [],
                                                     moduVerse = moduVerseInit,
                                                     junk = empty,
                                                     removeEmptyImports = True,
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
    do quietly (qLnPutStr ("unchanged: " ++ show _name))
       return x
doResult x@(Removed name) =
    do path <- modulePath name
       -- I think this event handler is redundant.
       removeFileIfPresent path `IO.catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
       delName name
       return x

doResult x@(Modified name text) =
    do path <- modulePath name
       qLnPutStr ("modifying " ++ show path)
       (quietly . quietly . quietly . qPutStr $ " new text: " ++ show text)
       replaceFile tildeBackup path text
       info <- parseModule path
       putName name info
       return x

doResult x@(Created name text) =
    do path <- modulePath name
       qLnPutStr ("creating " ++ show path)
       (quietly . quietly . quietly . qPutStr $ " containing " ++ show text)
       createDirectoryIfMissing True (takeDirectory . dropExtension $ path)
       replaceFile tildeBackup path text
       info <- parseModule path
       putName name info
       return x
