{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, RankNTypes #-}
-- | note clear if this is the right way to be able to use an
-- InputT (CleanT IO)@
module CLI.HaskelineTransAdapter () where
import System.Console.Haskeline.MonadException
import Control.Monad.Trans.Control
import Control.Monad
import Control.Monad.State
import Control.Monad.Base
import Control.Monad.Trans

instance (MonadBaseControl IO m, MonadIO m) => MonadException m where
   controlIO f = join $ liftBaseWith (\x -> (f (toRunIO x)))

toRunIO :: (MonadBaseControl b0 m) => RunInBase m IO -> RunIO m
toRunIO rb = RunIO (\a -> liftM restoreM (rb a))
