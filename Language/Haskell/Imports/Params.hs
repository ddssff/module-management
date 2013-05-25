{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports #-}
module Language.Haskell.Imports.Params
    ( MonadParams
    , runParamsT
    , dryRun
    , putDryRun
    , hsFlags
    , putHsFlags
    , markForDelete
    , putScratchJunk
    , scratchDir
    -- , putScratchDir
    , removeEmptyImports
    , putRemoveEmptyImports
    ) where

import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (MonadCatchIO, catch, throw)
import Control.Monad.State (MonadState(get, put), StateT(runStateT))
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Set (empty, insert, Set, toList)
import Data.String (fromString)
import Filesystem (removeTree, createTree)
import Language.Haskell.Imports.Common (removeFile')
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

data Params
    = Params
      { dryRun_ :: Bool
      , verbosity_ :: Int
      , hsFlags_ :: [String]
      , junk_ :: Set FilePath
      , scratchDir_ :: FilePath
      , removeEmpty_ :: Bool -- ^ If true, remove any import that
                             -- became empty due to the clean.  THe
                             -- import might still be required because
                             -- of the instances it contains, but
                             -- usually it is not.
      } deriving (Eq, Ord, Show)

class (MonadIO m, MonadCatchIO m) => MonadParams m where
    getParams :: m Params
    putParams :: Params -> m ()

modifyParams :: MonadParams m => (Params -> Params) -> m ()
modifyParams f = getParams >>= putParams . f

instance (MonadIO m, MonadCatchIO m) => MonadParams (StateT Params m) where
    getParams = get
    putParams = put

dryRun :: MonadParams m => m Bool
dryRun = getParams >>= return . dryRun_

putDryRun :: MonadParams m => Bool -> m ()
putDryRun x = modifyParams (\ p -> p {dryRun_ = x})

hsFlags :: MonadParams m => m [String]
hsFlags = getParams >>= return . hsFlags_

putHsFlags :: MonadParams m => [String] -> m ()
putHsFlags x = modifyParams (\ p -> p {hsFlags_ = x})

markForDelete :: MonadParams m => FilePath -> m ()
markForDelete x = modifyParams (\ p -> p {junk_ = insert x (junk_ p)})

putScratchJunk :: MonadParams m => FilePath -> m ()
putScratchJunk x = scratchDir >>= \ scratch -> markForDelete (scratch </> x)

runParamsT :: MonadIO m => FilePath -> StateT Params m a -> m a
runParamsT scratch action =
    do liftIO $ removeTree (fromString scratch) `catch` (\ e -> if isDoesNotExistError e then return () else throw e)
       liftIO $ createTree (fromString scratch)
       (result, params) <- runStateT action (Params {dryRun_ = False,
                                                     verbosity_ = 0,
                                                     hsFlags_ = [],
                                                     junk_ = empty,
                                                     scratchDir_ = scratch,
                                                     removeEmpty_ = True})
       mapM_ (liftIO . removeFile') (toList (junk_ params))
       return result

scratchDir :: MonadParams m => m FilePath
scratchDir = getParams >>= return . scratchDir_

-- putScratchDir :: MonadParams m => FilePath -> m ()
-- putScratchDir x = modifyParams (\ p -> p {scratchDir_ = x})

removeEmptyImports :: MonadParams m => m Bool
removeEmptyImports = getParams >>= return . removeEmpty_

putRemoveEmptyImports :: MonadParams m => Bool -> m ()
putRemoveEmptyImports x = modifyParams (\ p -> p {removeEmpty_ = x})
