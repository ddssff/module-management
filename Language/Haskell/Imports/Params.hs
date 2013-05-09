{-# LANGUAGE FlexibleInstances, PackageImports #-}
module Language.Haskell.Imports.Params
    ( MonadParams
    , runParamsT
    , dryRun
    , putDryRun
    , hsFlags
    , putHsFlags
    , junk
    , putJunk
    , putScratchJunk
    , scratchDir
    , putScratchDir
    ) where

import Data.Default (Default(def))
import Data.Set (Set, empty, insert)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (MonadCatchIO)
import Control.Monad.Trans (MonadIO)
import Control.Monad.State (StateT(runStateT), MonadState(get, put))
import System.FilePath ((</>))

data Params
    = Params
      { dryRun_ :: Bool
      , verbosity_ :: Int
      , hsFlags_ :: [String]
      , junk_ :: Set FilePath
      , scratchDir_ :: FilePath
      } deriving (Eq, Ord, Show)

instance Default Params where
    def = Params {dryRun_ = False, verbosity_ = 0, hsFlags_ = [], junk_ = empty, scratchDir_ = "dist/scratch"}

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

junk :: MonadParams m => m (Set FilePath)
junk = getParams >>= return . junk_

putJunk :: MonadParams m => FilePath -> m ()
putJunk x = modifyParams (\ p -> p {junk_ = insert x (junk_ p)})

putScratchJunk :: MonadParams m => FilePath -> m ()
putScratchJunk x = scratchDir >>= \ scratch -> modifyParams (\ p -> p {junk_ = insert (scratch </> x) (junk_ p)})

runParamsT :: MonadIO m => StateT Params m a -> m a
runParamsT action = runStateT action def >>= return . fst

scratchDir :: MonadParams m => m FilePath
scratchDir = getParams >>= return . scratchDir_

putScratchDir :: MonadParams m => FilePath -> m ()
putScratchDir x = modifyParams (\ p -> p {scratchDir_ = x})
