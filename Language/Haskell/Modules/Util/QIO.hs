module Language.Haskell.Modules.Util.QIO
    ( MonadVerbosity(getVerbosity, putVerbosity)
    , modifyVerbosity
    , quietly
    , noisily
    , qIO
    , qPutStr
    , qPutStrLn
    ) where

import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)

class MonadIO m => MonadVerbosity m where
    getVerbosity :: m Int
    putVerbosity :: Int -> m ()

modifyVerbosity :: MonadVerbosity m => (Int -> Int) -> m ()
modifyVerbosity f = getVerbosity >>= putVerbosity . f

quietly :: MonadVerbosity m => m a -> m a
quietly action =
    do modifyVerbosity ((-) 1)
       result <- action
       modifyVerbosity (+ 1)
       return result

noisily :: MonadVerbosity m => m a -> m a
noisily action =
    do modifyVerbosity (+ 1)
       result <- action
       modifyVerbosity ((-) 1)
       return result

qIO :: MonadVerbosity m => m () -> m ()
qIO action =
    do v <- getVerbosity
       when (v > 0) action

qPutStrLn :: MonadVerbosity m => String -> m ()
qPutStrLn = qIO . liftIO . putStrLn

qPutStr :: MonadVerbosity m => String -> m ()
qPutStr = qIO . liftIO . putStr
