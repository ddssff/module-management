{-# LANGUAGE FlexibleInstances, PackageImports #-}
module Language.Haskell.Imports.Params
    ( Params(..)
    , MonadParams(askParams)
    , runParamsT
    ) where

import Data.Default (Default(def))
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (MonadCatchIO)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask))

data Params
    = Params
      { dryRun :: Bool
      , verbosity :: Int
      , hsFlags :: [String]
      } deriving (Eq, Ord, Show)

instance Default Params where
    def = Params {dryRun = False, verbosity = 0, hsFlags = []}

class (MonadIO m, MonadCatchIO m) => MonadParams m where
    askParams :: m Params

instance (MonadIO m, MonadCatchIO m) => MonadParams (ReaderT Params m) where
    askParams = ask

runParamsT :: MonadIO m => Params -> ReaderT Params m a -> m a
runParamsT params action = runReaderT action params
