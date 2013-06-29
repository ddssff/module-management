{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Modules.Internal
    ( runMonadClean
    , modifyParams
    , parseFileWithComments
    , parseFile
    , modulePath
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
import Data.Set (empty, insert, Set, toList)
import qualified Language.Haskell.Exts.Annotated as A (Module, parseFileWithComments, parseFileWithMode)
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Extension (Extension)
import qualified Language.Haskell.Exts.Parser as Exts (defaultParseMode, ParseMode(extensions), ParseResult)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName)
import Language.Haskell.Modules.Common (modulePathBase)
import Language.Haskell.Modules.Util.DryIO (createDirectoryIfMissing, MonadDryRun(..), removeFileIfPresent, replaceFile, tildeBackup)
import Language.Haskell.Modules.Util.QIO (MonadVerbosity(..), qPutStr, qPutStrLn, quietly)
import Language.Haskell.Modules.Util.Temp (withTempDirectory)
import Prelude hiding (writeFile)
import System.Directory (doesFileExist, getCurrentDirectory, removeFile)
import System.FilePath ((</>), dropExtension, takeDirectory)
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
      , extensions :: [Extension]
      -- ^ Supply compiler extensions.  These are provided to the module
      -- parser and to GHC when it does the minimal import dumping.
      , hsFlags :: [String]
      -- ^ Extra flags to pass to GHC.
      , sourceDirs :: [FilePath]
      -- ^ Top level directories to search for source files and
      -- imports.  These directories would be the value used in the
      -- hs-source-dirs parameter of a cabal file, and passed to ghc
      -- via the -i option.
      , moduVerse :: Maybe (Set S.ModuleName)
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
                                                     extensions = Exts.extensions Exts.defaultParseMode,
                                                     sourceDirs = ["."],
                                                     moduVerse = Nothing,
                                                     junk = empty,
                                                     removeEmptyImports = True,
                                                     testMode = False})
       mapM_ (\ x -> liftIO (try (removeFile x)) >>= \ (_ :: Either SomeException ()) -> return ()) (toList (junk params))
       return result

-- | Run 'A.parseFileWithComments' with the extensions stored in the state.
parseFileWithComments :: MonadClean m => FilePath -> m (Exts.ParseResult (A.Module SrcSpanInfo, [Comment]))
parseFileWithComments path =
    do exts <- getParams >>= return . extensions
       liftIO (A.parseFileWithComments (Exts.defaultParseMode {Exts.extensions = exts}) path)

-- | Run 'A.parseFileWithMode' with the extensions stored in the state.
parseFile :: MonadClean m => FilePath -> m (Exts.ParseResult (A.Module SrcSpanInfo))
parseFile path =
    do exts <- getParams >>= return . extensions
       liftIO (A.parseFileWithMode (Exts.defaultParseMode {Exts.extensions = exts}) path)

-- | Search the path directory list for a source file that already exists.
findSourcePath :: MonadClean m => FilePath -> m FilePath
findSourcePath path =
    findFile =<< (sourceDirs <$> getParams)
    where
      findFile (dir : dirs) =
          do let x = dir </> path
             exists <- liftIO $ doesFileExist x
             if exists then return x else findFile dirs
      findFile [] =
          do -- Just building an error message here
             here <- liftIO getCurrentDirectory
             dirs <- sourceDirs <$> getParams
             liftIO . throw . userError $ "findSourcePath failed, cwd=" ++ here ++ ", dirs=" ++ show dirs ++ ", path=" ++ path

-- | Search the path directory list, preferring an already existing file, but
-- if there is none construct one using the first element of the directory list.
modulePath :: MonadClean m => S.ModuleName -> m FilePath
modulePath name =
    findSourcePath (modulePathBase name) `catch` (\ (_ :: IOError) -> makePath)
    where
      makePath =
          do dirs <- sourceDirs <$> getParams
             case dirs of
               [] -> return (modulePathBase name) -- should this be an error?
               (d : _) -> return $ d </> modulePathBase name

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
doResult :: MonadClean m => ModuleResult -> m ModuleResult
doResult x@(Unchanged _name) =
    do quietly (qPutStrLn ("unchanged: " ++ show _name))
       return x
doResult x@(Removed name) =
    do path <- modulePath name
       -- I think this event handler is redundant.
       removeFileIfPresent path `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
       return x

doResult x@(Modified name text) =
    do path <- modulePath name
       qPutStr ("modifying " ++ show path)
       quietly (qPutStr (" new text: " ++ show text))
       qPutStr "\n"
       replaceFile tildeBackup path text
       return x

doResult x@(Created name text) =
    do path <- modulePath name
       qPutStr ("creating " ++ show path)
       quietly (qPutStr (" containing " ++ show text))
       qPutStr "\n"
       createDirectoryIfMissing True (takeDirectory . dropExtension $ path)
       replaceFile tildeBackup path text
       return x
