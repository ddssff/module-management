{-# LANGUAGE ScopedTypeVariables #-}
module Debian.Repo.Prelude.Lock
    ( withLock
    , awaitLock
    ) where

import Control.Exception
import Control.Monad.RWS
import System.Directory
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Posix.IO
import System.Posix.Unistd

withLock :: (MonadIO m) => FilePath -> m a -> m a
withLock path task =
    liftIO (checkLock >> takeLock) >> task >>= \ result -> liftIO dropLock >> return result
    where
      -- Return True if file is locked by a running process, false otherwise
      --checkLock :: IO (Either Exception ())
      checkLock = readFile path `catch` checkReadError >>= processRunning . lines
      checkReadError :: IOError -> IO String
      checkReadError e | isDoesNotExistError e = return ""
      checkReadError e = throw e
      processRunning :: [String] -> IO ()
      processRunning (pid : _) =
          do exists <- doesDirectoryExist ("/proc/" ++ pid)
             case exists of
               True -> throw (lockedBy pid path)
               False -> breakLock
      processRunning [] = breakLock
      breakLock = removeFile path `catch` checkBreakError
      checkBreakError (e :: IOException) | isDoesNotExistError e = return ()
      checkBreakError e = throw e
      takeLock :: IO ()
      takeLock =
          -- Try to create the lock file in exclusive mode, if this
          -- succeeds then we have a lock.  Then write the process ID
          -- into the lock and close.
          openFd path ReadWrite (Just 0o600) (defaultFileFlags {exclusive = True, trunc = True}) >>=
          fdToHandle >>= \ h -> processID >>= hPutStrLn h >> hClose h
      dropLock = removeFile path `catch` checkDrop
      checkDrop (e :: IOException) | isDoesNotExistError e = return ()
      checkDrop e = throw e

-- |Like withLock, but instead of giving up immediately, try n times
-- with a wait between each.
awaitLock :: Int -> Int -> FilePath -> IO a -> IO a
awaitLock tries usecs path task =
    attempt 0
    where
      attempt n | n >= tries = error "awaitLock: too many failures"
      attempt n = withLock path task `catch` checkLockError
          where
            checkLockError e | isAlreadyInUseError e = liftIO (usleep usecs) >> attempt (n + 1)
            checkLockError e = throw e

processID :: IO String
processID = readSymbolicLink "/proc/self"

lockedBy :: String -> FilePath -> IOError
lockedBy pid path = mkIOError alreadyInUseErrorType ("Locked by " ++ pid) Nothing (Just path)
