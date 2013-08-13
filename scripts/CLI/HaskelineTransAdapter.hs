{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}
-- | note clear if this is the right way to be able to use an
-- InputT (CleanT IO)@
module CLI.HaskelineTransAdapter () where
import System.Console.Haskeline.MonadException
import System.Console.Haskeline
import Control.Monad.Trans.Control
import Control.Monad
import Control.Monad.State
import Control.Monad.Base
import Control.Monad.Trans
import Language.Haskell.Modules
import System.Console.Haskeline.History

import Data.IORef
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import System.IO.Unsafe
import Unsafe.Coerce
import Control.Monad.Reader

-- * Accessing un-exported identifiers
mkInputT x = $(conE $ mkNameG DataName "haskeline-0.7.0.3" "System.Console.Haskeline.InputT" "InputT") x
unInputT x = $(varE $ mkNameG VarName "haskeline-0.7.0.3" "System.Console.Haskeline.InputT" "unInputT") x

-- type KR = KillRing -- isn't exported
fmap (:[]) $ tySynD (mkName "KR") [] (conT $ mkNameG TcClsName "haskeline-0.7.0.3" "System.Console.Haskeline.Command.KillRing" "KillRing")

instance MonadBase b m => MonadBase b (InputT m) where
    liftBase = liftBaseDefault

instance MonadTransControl InputT where
    newtype StT InputT a = StInputT { unStInput :: (IORef History, IORef KR,a) }
    liftWith fn = mkInputT $ ReaderT $ \r1 -> ReaderT $ \r2 -> ReaderT $ \r3 -> ReaderT $ \r4 -> ReaderT $ \r5 -> do
        (r2',r3') <- return $ unsafePerformIO $ liftM2 (,)
                (newIORef =<< readIORef r2)
                (newIORef =<< readIORef r3)
        let (^) = runReaderT

        fn $ \t -> do
            -- without the unsafeCoerce (using noCompletion instead) means the hmm program
            -- doesn't give any completions
            a <- unInputT t ^ r1 ^ r2' ^ r3' ^ r4 ^ r5{ complete = unsafeCoerce $ complete r5}
            return (StInputT (r2', r3', a))

    restoreT x = mkInputT $ ReaderT $ \r1 -> ReaderT $ \r2 -> ReaderT $ \r3 -> ReaderT $ \r4 -> ReaderT $ \r5 -> do
        (r2',r3', a) <- liftM unStInput x

        return $! ((unsafePerformIO $ do
                        writeIORef r2 =<< readIORef r2'
                        writeIORef r3 =<< readIORef r3') `seq` a)



instance MonadBaseControl b m => MonadBaseControl b (InputT m) where
    data StM (InputT m) a = StMT {unStMT :: ComposeSt InputT m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM   unStMT

instance (MonadBaseControl IO m, MonadIO m) => MonadException m where
   controlIO f = join $ liftBaseWith (\x -> (f (toRunIO x)))

toRunIO :: (MonadBaseControl b0 m) => RunInBase m IO -> RunIO m
toRunIO rb = RunIO (\a -> liftM restoreM (rb a))
