{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables #-}
module Language.Haskell.Modules.Params
    ( Params(..)
    , MonadClean
    , runCleanT
    , getParams
    , modifyParams
    , parseFileWithComments
    , parseFileWithMode
    , findSourcePath
    , modulePath
    , markForDelete
    , putScratchJunk
    , quietly
    , noisily
    , qIO
    , qPutStr
    , qPutStrLn
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, MonadCatchIO, throw)
import Control.Monad.State (MonadState(get, put), StateT(runStateT))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Set (empty, insert, Set, toList)
import Data.String (fromString)
import Filesystem (createTree, removeTree)
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Extension (Extension)
import Language.Haskell.Exts.Parser as Exts (ParseMode(extensions), defaultParseMode, ParseResult)
import qualified Language.Haskell.Exts.Annotated as Exts (parseFileWithMode, parseFileWithComments)
import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Modules.Common (Module, removeFileIfPresent, modulePathBase)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

data Params
    = Params
      { scratchDir :: FilePath
      -- ^ Location of the scratch directory for ghc output.  Cabal
      -- uses dist/scratch by default.
      , dryRun :: Bool -- unimplemented
      , verbosity :: Int
      , extensions :: [Extension]
      , hsFlags :: [String]
      -- ^ Extra flags to pass to GHC.
      , sourceDirs :: [FilePath]
      -- ^ Top level directories to search for source files and
      -- imports.  These directories would be the value used in the
      -- hs-source-dirs parameter of a cabal file, and passed to ghc
      -- via the -i option.
      , junk :: Set FilePath
      -- ^ Paths added to this list are removed as the monad finishes.
      , removeEmptyImports :: Bool
      -- ^ If true, remove any import that became empty due to the
      -- clean.  THe import might still be required because of the
      -- instances it contains, but usually it is not.  Note that this
      -- option does not affect imports that started empty and end
      -- empty.
      } deriving (Eq, Ord, Show)

class (MonadIO m, MonadCatchIO m, Functor m) => MonadClean m where
    getParams :: m Params
    putParams :: Params -> m ()

modifyParams :: MonadClean m => (Params -> Params) -> m ()
modifyParams f = getParams >>= putParams . f

instance (MonadIO m, MonadCatchIO m, Functor m) => MonadClean (StateT Params m) where
    getParams = get
    putParams = put

runCleanT :: MonadIO m => FilePath -> StateT Params m a -> m a
runCleanT scratch action =
    do liftIO $ removeTree (fromString scratch) `catch` (\ e -> if isDoesNotExistError e then return () else throw e)
       liftIO $ createTree (fromString scratch)
       (result, params) <- runStateT action (Params {scratchDir = scratch,
                                                     dryRun = False,
                                                     verbosity = 0,
                                                     hsFlags = [],
                                                     Language.Haskell.Modules.Params.extensions = Exts.extensions defaultParseMode,
                                                     sourceDirs = ["."],
                                                     junk = empty,
                                                     removeEmptyImports = True})
       mapM_ (liftIO . removeFileIfPresent) (toList (junk params))
       return result

parseFileWithComments :: MonadClean m => FilePath -> m (ParseResult (Module, [Comment]))
parseFileWithComments path =
    do exts <- getParams >>= return . Language.Haskell.Modules.Params.extensions
       liftIO (Exts.parseFileWithComments (defaultParseMode {Exts.extensions = exts}) path)

parseFileWithMode :: MonadClean m => FilePath -> m (ParseResult Module)
parseFileWithMode path =
    do exts <- getParams >>= return . Language.Haskell.Modules.Params.extensions
       liftIO (Exts.parseFileWithMode (defaultParseMode {Exts.extensions = exts}) path)

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
          do here <- liftIO getCurrentDirectory
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
               [] -> return (modulePathBase name)
               (d : _) -> return $ d </> modulePathBase name

markForDelete :: MonadClean m => FilePath -> m ()
markForDelete x = modifyParams (\ p -> p {junk = insert x (junk p)})

putScratchJunk :: MonadClean m => FilePath -> m ()
putScratchJunk x = scratchDir <$> getParams >>= \ scratch -> markForDelete (scratch </> x)

quietly :: MonadClean m => m a -> m a
quietly action =
    do modifyParams (\ p -> p {verbosity = verbosity p - 1})
       result <- action
       modifyParams (\ p -> p {verbosity = verbosity p + 1})
       return result

noisily :: MonadClean m => m a -> m a
noisily action =
    do modifyParams (\ p -> p {verbosity = verbosity p + 1})
       result <- action
       modifyParams (\ p -> p {verbosity = verbosity p - 1})
       return result

qIO :: MonadClean m => m () -> m ()
qIO action =
    do v <- verbosity <$> getParams
       when (v > 0) action

qPutStrLn :: MonadClean m => String -> m ()
qPutStrLn = qIO . liftIO . putStrLn

qPutStr :: MonadClean m => String -> m ()
qPutStr = qIO . liftIO . putStr
