module Language.Haskell.Modules.Util.Temp
    ( withTempDirectory
    ) where

import qualified Control.Exception as IO (catch)
import Control.Monad.CatchIO as IOT (bracket, MonadCatchIO)
import Control.Monad.Trans (liftIO, MonadIO)
import System.Directory (removeDirectoryRecursive)
import qualified System.IO.Temp as Temp (createTempDirectory)

-- | Adapted from 'System.IO.Temp.withTempDirectory' to work in MonadCatchIO instances.
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
