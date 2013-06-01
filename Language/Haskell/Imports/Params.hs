{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports #-}
module Language.Haskell.Imports.Params
    ( MonadClean
    , runCleanT
    , dryRun
    -- , putDryRun
    , hsFlags
    , putHsFlags
    , sourceDirs
    , putSourceDirs
    , findSourcePath
    , markForDelete
    , putScratchJunk
    , scratchDir
    -- , putScratchDir
    , removeEmptyImports
    , putRemoveEmptyImports
    ) where

import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, MonadCatchIO, throw)
import Control.Monad.State (MonadState(get, put), StateT(runStateT))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Set (empty, insert, Set, toList)
import Data.String (fromString)
import Filesystem (createTree, removeTree)
import Language.Haskell.Imports.Common (removeFile')
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

data Params
    = Params
      { scratchDir_ :: FilePath
      -- ^ Location of the scratch directory for ghc output.  Cabal
      -- uses dist/scratch by default.
      -- , dryRun_ :: Bool -- unimplemented
      -- , verbosity_ :: Int -- unimplemented
      , hsFlags_ :: [String]
      -- ^ Extra flags to pass to GHC.
      , sourceDirs_ :: [FilePath]
      -- ^ Top level directories to search for source files and
      -- imports.  These directories would be the value used in the
      -- hs-source-dirs parameter of a cabal file, and passed to ghc
      -- via the -i option.
      , junk_ :: Set FilePath
      -- ^ Paths added to this list are removed as the monad finishes.
      , removeEmpty_ :: Bool
      -- ^ If true, remove any import that became empty due to the
      -- clean.  THe import might still be required because of the
      -- instances it contains, but usually it is not.  Note that this
      -- option does not affect imports that started empty and end
      -- empty.
      } deriving (Eq, Ord, Show)

class (MonadIO m, MonadCatchIO m) => MonadClean m where
    getParams :: m Params
    putParams :: Params -> m ()

modifyParams :: MonadClean m => (Params -> Params) -> m ()
modifyParams f = getParams >>= putParams . f

instance (MonadIO m, MonadCatchIO m) => MonadClean (StateT Params m) where
    getParams = get
    putParams = put

dryRun :: MonadClean m => m Bool
-- dryRun = getParams >>= return . dryRun_
dryRun = return False

{-
putDryRun :: MonadClean m => Bool -> m ()
putDryRun x = modifyParams (\ p -> p {dryRun_ = x})
-}

hsFlags :: MonadClean m => m [String]
hsFlags = getParams >>= return . hsFlags_

putHsFlags :: MonadClean m => [String] -> m ()
putHsFlags x = modifyParams (\ p -> p {hsFlags_ = x})

putSourceDirs :: MonadClean m => [FilePath] -> m ()
putSourceDirs xs = modifyParams (\ p -> p {sourceDirs_ = xs})

sourceDirs :: MonadClean m => m [FilePath]
sourceDirs = getParams >>= return . sourceDirs_

findSourcePath :: MonadClean m => FilePath -> m FilePath
findSourcePath path =
    do dirs <- sourceDirs
       findFile dirs
    where
      findFile [] = liftIO . throw . userError $ "Not found: " ++ path
      findFile (dir : dirs) =
          do let x = dir </> path
             exists <- liftIO $ doesFileExist x
             if exists then return x else findFile dirs

markForDelete :: MonadClean m => FilePath -> m ()
markForDelete x = modifyParams (\ p -> p {junk_ = insert x (junk_ p)})

putScratchJunk :: MonadClean m => FilePath -> m ()
putScratchJunk x = scratchDir >>= \ scratch -> markForDelete (scratch </> x)

runCleanT :: MonadIO m => FilePath -> StateT Params m a -> m a
runCleanT scratch action =
    do liftIO $ removeTree (fromString scratch) `catch` (\ e -> if isDoesNotExistError e then return () else throw e)
       liftIO $ createTree (fromString scratch)
       (result, params) <- runStateT action (Params {scratchDir_ = scratch,
                                                     -- dryRun_ = False,
                                                     -- verbosity_ = 0,
                                                     hsFlags_ = [],
                                                     sourceDirs_ = ["."],
                                                     junk_ = empty,
                                                     removeEmpty_ = True})
       mapM_ (liftIO . removeFile') (toList (junk_ params))
       return result

scratchDir :: MonadClean m => m FilePath
scratchDir = getParams >>= return . scratchDir_

-- putScratchDir :: MonadClean m => FilePath -> m ()
-- putScratchDir x = modifyParams (\ p -> p {scratchDir_ = x})

removeEmptyImports :: MonadClean m => m Bool
removeEmptyImports = getParams >>= return . removeEmpty_

putRemoveEmptyImports :: MonadClean m => Bool -> m ()
putRemoveEmptyImports x = modifyParams (\ p -> p {removeEmpty_ = x})
