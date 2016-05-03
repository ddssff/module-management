{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Util.DryIO
    ( MonadDryRun(..)
    , dryIO
    , dryIO'
    , tildeBackup
    , noBackup
    , readFileMaybe
    , replaceFile
    , replaceFileIfDifferent
    , removeFileIfPresent
    , createDirectoryIfMissing
    , writeFile
    ) where

import Control.Exception as E (catch, throw)
import Control.Monad.Trans (liftIO, MonadIO)
import Prelude hiding (writeFile)
import System.Directory (removeFile, renameFile)
import qualified System.Directory as IO (createDirectoryIfMissing)
import qualified System.IO as IO (writeFile)
import System.IO.Error (isDoesNotExistError)

tildeBackup :: FilePath -> Maybe FilePath
tildeBackup = Just . (++ "~")

noBackup :: FilePath -> Maybe FilePath
noBackup = const Nothing

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path = (Just <$> readFile path) `E.catch` (\ (e :: IOError) -> if isDoesNotExistError e then return Nothing else throw e)

removeFileIfPresent :: MonadDryRun m => FilePath -> m ()
removeFileIfPresent path = dryIO' (putStrLn $ "dry run: removeFileIfPresent " ++ path) (removeFile path `E.catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e))

replaceFileIfDifferent :: MonadDryRun m => FilePath -> String -> m Bool
replaceFileIfDifferent path newText =
    do oldText <- liftIO $ readFileMaybe path
       if oldText == Just newText then return False else replaceFile tildeBackup path newText >> return True

-- | Replace the file at path with the given text, moving the original
-- to the location returned by passing path to backup.  If backup is
-- the identity function you're going to have a bad time.
replaceFile :: MonadDryRun m => (FilePath -> Maybe FilePath) -> FilePath -> String -> m ()
replaceFile backup path text =
    dryIO' (putStrLn $ "dry run: replaceFile " ++ path) (remove >> rename >> write)
    where
      remove = maybe (return ()) removeFile (backup path) `E.catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      rename = maybe (return ()) (renameFile path) (backup path) `E.catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      write = IO.writeFile path text

createDirectoryIfMissing :: MonadDryRun m => Bool -> String -> m ()
createDirectoryIfMissing flag path =
    dryIO' (putStrLn $ "dry run: createDirectoryIfMissing " ++ show flag ++ " " ++ path) (IO.createDirectoryIfMissing flag path)

writeFile :: MonadDryRun m => FilePath -> String -> m ()
writeFile path text =
    dryIO' (putStrLn $ "dry run: writeFIle " ++ path) (IO.writeFile path text)

class MonadIO m => MonadDryRun m where
    dry :: m Bool
    putDry :: Bool -> m ()

dryIO :: MonadDryRun m => IO () -> m ()
dryIO action = dryIO' (return ()) action

dryIO' :: MonadDryRun m => IO a -> IO a -> m a
dryIO' d action =
    do flag <- dry
       if flag then liftIO d else liftIO action
