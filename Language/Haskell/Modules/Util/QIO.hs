-- | IO operations predicated on the verbosity value managed by the
-- methods of MonadVerbosity.  Noisily increases this value and
-- quietly decreases it, and the q* operations only happen when the
-- value is greater than zero.
module Language.Haskell.Modules.Util.QIO
    ( MonadVerbosity(getVerbosity, putVerbosity)
    , modifyVerbosity
    , quietly
    , noisily
    , qIO
    , qPutStr
    , qPutStrLn
    , qLnPutStr
    ) where

import Control.Monad (when)
import Control.Monad.Trans (liftIO, MonadIO)

class MonadIO m => MonadVerbosity m where
    getVerbosity :: m Int
    putVerbosity :: Int -> m ()

modifyVerbosity :: MonadVerbosity m => (Int -> Int) -> m ()
modifyVerbosity f = getVerbosity >>= putVerbosity . f

quietly :: MonadVerbosity m => m a -> m a
quietly action =
    do modifyVerbosity (\x->x-1)
       result <- action
       modifyVerbosity (+ 1)
       return result

noisily :: MonadVerbosity m => m a -> m a
noisily action =
    do modifyVerbosity (+ 1)
       result <- action
       modifyVerbosity (\x->x-1)
       return result

qIO :: MonadVerbosity m => m () -> m ()
qIO action =
    do v <- getVerbosity
       when (v > 0) action

qPutStr :: MonadVerbosity m => String -> m ()
qPutStr = qIO . liftIO . putStr

qPutStrLn :: MonadVerbosity m => String -> m ()
qPutStrLn s =
    qIO $ do v <- getVerbosity
             liftIO $ putStrLn (replicate (5 - min 5 v) ' ' ++ s)

qLnPutStr :: MonadVerbosity m => String -> m ()
qLnPutStr s =
    qIO $ do v <- getVerbosity
             liftIO $ putStr  ("\n" ++ replicate (5 - min 5 v) ' ' ++ s)
