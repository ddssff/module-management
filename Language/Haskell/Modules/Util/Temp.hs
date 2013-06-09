module Language.Haskell.Modules.Util.Temp
    ( withTempDirectory
    ) where

import System.Directory (removeDirectoryRecursive)
import qualified Control.Exception as IO
import Control.Monad.CatchIO as IOT (MonadCatchIO, bracket)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified System.IO.Temp as Temp

-- | Adapted from 'System.IO.Temp.withTempDirectory'.
withTempDirectory :: MonadCatchIO m =>
                     FilePath -- ^ Temp directory to create the directory in
                  -> String   -- ^ Directory name template. See 'openTempFile'.
                  -> (FilePath -> m a) -- ^ Callback that can use the directory
                  -> m a
withTempDirectory targetDir template =
    IOT.bracket
       (liftIO $ Temp.createTempDirectory targetDir template)
       (liftIO . ignoringIOErrors . removeDirectoryRecursive)

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `IO.catch` (\e -> const (return ()) (e :: IOError))
