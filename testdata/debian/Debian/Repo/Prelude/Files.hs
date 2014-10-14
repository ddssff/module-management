{-# LANGUAGE ScopedTypeVariables #-}
-- |Some extra operations on files.  The functions here generally
-- return (Right ()) on success, Left [messages] on failure, and throw
-- an exception when a failure leaves things in an inconsistant state.
-- An example of an inconsistant state would be if we got a failure
-- when writing out a file, but were unable to restore the original
-- file to its original position.
module Debian.Repo.Prelude.Files
    ( getSubDirectories
    , renameAlways
    , renameMissing
    , deleteMaybe
    , installFiles
    , writeAndZipFileWithBackup
    , writeAndZipFile
    , backupFile
    , writeFileIfMissing
    , maybeWriteFile		-- writeFileUnlessSame
    , createSymbolicLinkIfMissing
    , prepareSymbolicLink
    , forceRemoveLink
    , replaceFile
    ) where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import		 Control.Exception as E
import		 Control.Monad
import qualified Data.ByteString.Lazy as B
import		 Data.List
import		 Data.Maybe
import Debian.Repo.Prelude.Misc (parentPath)
import		 System.Unix.Directory
import		 System.Directory
import		 System.IO.Error
import		 System.Posix.Files

-- | Return the list of subdirectories, omitting . and .. and ignoring
-- symbolic links.
getSubDirectories :: FilePath -> IO [String]
getSubDirectories path =
    getDirectoryContents path >>=
    return . filter (not . (flip elem) [".", ".."]) >>=
    filterM isRealDirectory
    where
      isRealDirectory name = getSymbolicLinkStatus (path ++ "/" ++ name) >>= return . not . isSymbolicLink

-- |Atomically install a list of files.  Returns a list of what went
-- wrong on failure.  Will throw an error if it fails and is unable to
-- restore the original files to their original states.
installFiles :: [(FilePath, FilePath)] -> IO (Either [String] ())
installFiles pairs =
    do backedUp <- mapM (uncurry renameAlways) (zip originalFiles backupFiles)
       case lefts backedUp of
         [] -> 
             do renamed <- mapM (uncurry renameAlways) (zip replacementFiles originalFiles)
                case lefts renamed of
                  [] -> return $ Right ()
                  _ ->
                      -- We failed after all the original files were
                      -- renamed and maybe some of the replacement
                      -- files were installed.  Move all the renamed
                      -- files back into place.
                      do restored <- mapM (uncurry renameAlways) (zip backupFiles originalFiles)
                         case lefts restored of
	                   -- We succeeded in failing.
                           [] -> return . Left . concat . lefts $ renamed
	                   -- Restore failed.  Throw an exception.
                           _ -> error ("installFiles: Couldn't restore original files after write failure:" ++
                                       concat (map message (zip3 replacementFiles originalFiles renamed)) ++
                                       concat (map message (zip3 originalFiles backupFiles restored)))
         _ ->
             -- We failed after renaming all original files, but
             -- before any of the replacement files were installed.
             -- Restore the backup for any missing original files.
             do restored <- mapM (uncurry renameMissing) (zip backupFiles originalFiles)
                case lefts restored of
	          -- We succeeded in failing.
                  [] -> return . Left . concat . lefts $ backedUp
		  -- Restore failed.  Throw an exception.
                  _ -> error ("installFiles: Couldn't restore original files after write failure: " ++
                              concat (map message (zip3 originalFiles backupFiles backedUp)) ++
                              concat (map message (zip3 backupFiles originalFiles restored)))
    where
      replacementFiles = map fst pairs
      originalFiles = map snd pairs
      backupFiles = map (++ "~") originalFiles
      message (path1, path2, Left problems) =
          "\n  " ++ path1 ++ " -> " ++ path2 ++ ": " ++ concat (intersperse ", " (map show problems))
      message (_, _, Right ()) = ""

lefts :: [Either a b] -> [a]
lefts xs = catMaybes $ map (either Just (const Nothing)) xs

-- |Change a file's name only if the new name doesn't exist.
renameMissing :: FilePath -> FilePath -> IO (Either [String] ())
renameMissing old new =
    do exists <- fileExist new
       case exists of
         True -> return $ Right ()
         False -> renameAlways old new

-- |Change a file's name, removing any existing file with the new name.
renameAlways :: FilePath -> FilePath -> IO (Either [String] ())
renameAlways old new =
    do deleted <- deleteMaybe new
       case deleted of
         Right () ->
             E.try (rename old new) >>=
             return . either (\ (e :: SomeException) -> Left ["Couldn't rename " ++ old ++ " -> " ++ new ++ ": " ++ show e]) (\ _ -> Right ())
         x -> return x

-- |Change a file's name if it exists.
renameMaybe :: FilePath -> FilePath -> IO (Either [String] ())
renameMaybe old new =
    do exists <- fileExist old
       case exists of
         False -> return $ Right ()
         True -> renameAlways old new

-- |Delete a file if it exists
deleteMaybe :: FilePath -> IO (Either [String] ())
deleteMaybe path =
    do exists <- fileExist path
       case exists of
         False -> return $ Right ()
         True ->
             do status <- getSymbolicLinkStatus path
		-- To do: should we remove the directory contents?
                let rm = if isDirectory status then removeDirectory else removeLink
                try (rm path) >>= return . either (\ (e :: SomeException) -> Left ["Couldn't remove " ++ path ++ ": " ++ show e]) (const . Right $ ())

-- |Create or update gzipped and bzip2-ed versions of a file.
zipFile :: FilePath -> IO (Either [String] ())
zipFile path =
    try (do forceRemoveLink gz
            forceRemoveLink bz2
            B.readFile path >>= B.writeFile gz . {- t1 . -} GZip.compress
            B.readFile path >>= B.writeFile bz2 . {- t2 . -} BZip.compress) >>=
    return . either (\ (e :: SomeException) -> Left ["Failure writing and zipping " ++ path, show e]) Right
    where
      gz = path ++ ".gz"
      bz2 = path ++ ".bz2"
      --t1 s = trace ("Size of " ++ gz ++ " text: " ++ show (L.length s)) s
      --t2 s = trace ("Size of " ++ bz2 ++ " text: " ++ show (L.length s)) s
      --writeMessage (command, output) =
      --    case exitCodeOnly output of
      --      (ExitFailure n : _) ->
      --          command ++ " -> " ++ show n ++ ":\n  " ++ L.unpack (stderrOnly output)
      --      _ -> ""

-- |like removeLink, but does not fail if link did not exist
forceRemoveLink :: FilePath -> IO ()
forceRemoveLink fp = removeLink fp `E.catch` (\e -> unless (isDoesNotExistError e) (ioError e))

-- | Write out three versions of a file, regular, gzipped, and bzip2ed.
writeAndZipFileWithBackup :: FilePath -> B.ByteString -> IO (Either [String] ())
writeAndZipFileWithBackup path text =
    backupFile path >>=
    either (\ e -> return (Left ["Failure renaming " ++ path ++ " -> " ++ path ++ "~: " ++ show e]))
           (\ _ -> try (B.writeFile path text) >>=
                   either (\ (e :: SomeException) ->
                               restoreBackup path >>=
                               either (\ e' -> error ("Failed to restore backup: " ++ path ++ "~ -> " ++ path ++ ": " ++ show e'))
                                      (\ _ -> return (Left ["Failure writing " ++ path ++ ": " ++ show e])))
                          (\ _ -> zipFile path))

-- | Write out three versions of a file, regular, gzipped, and bzip2ed.
-- This new version assumes the files are written to temporary locations,
-- so any existing file there can be removed.
writeAndZipFile :: FilePath -> B.ByteString -> IO (Either [String] ())
writeAndZipFile path text =
    deleteMaybe path >>=
    either (\ e -> return (Left ["Failure removing " ++ path ++ ": " ++ show e]))
           (\ _ -> try (B.writeFile path text) >>=
                   either (\ (e :: SomeException) -> return (Left ["Failure writing " ++ path ++ ": " ++ show e]))
                          (\ _ -> zipFile path))

-- Turn a file into a backup file if it exists.
backupFile :: FilePath -> IO (Either [String] ())
backupFile path = renameMaybe path (path ++ "~")

restoreBackup :: FilePath -> IO (Either [String] ())
restoreBackup path = renameMaybe (path ++ "~") path

-- | Like writeFile, but if the file already exists don't touch it.
-- Example: writeFileIfMissing True \"\/var\/lib\/dpkg\/status\" \"\"
writeFileIfMissing :: Bool -> FilePath -> String -> IO ()
writeFileIfMissing mkdirs path text =
    do
      exists <- doesFileExist path
      case exists of
        False ->
            do
              if mkdirs then
                  createDirectoryIfMissing True (parentPath path) else
                  return ()
              replaceFile path text
        True ->
            return ()

-- | Write a file if its content is different from the given text.
maybeWriteFile :: FilePath -> String -> IO ()
maybeWriteFile path text =
    try (readFile path) >>= maybeWrite
    where
      maybeWrite (Left (e :: IOException)) | isDoesNotExistError e = writeFile path text
      maybeWrite (Left e) = error ("maybeWriteFile: " ++ show e)
      maybeWrite (Right old) | old == text = return ()
      maybeWrite (Right _old) = 
          --hPutStrLn stderr ("Old text: " ++ show old) >>
          --hPutStrLn stderr ("New text: " ++ show text) >>
          replaceFile path text

-- |Add-on for System.Posix.Files
createSymbolicLinkIfMissing :: String -> FilePath -> IO ()
createSymbolicLinkIfMissing text path =
    try (getSymbolicLinkStatus path) >>=
    either (\ (_ :: SomeException) -> createSymbolicLink text path) (\ _ -> return ())

prepareSymbolicLink :: FilePath -> FilePath -> IO ()
prepareSymbolicLink name path =
    checkExists >>= checkType >>= checkContent
    where
      checkExists = doesDirectoryExist path >>= orCreate
      checkType False = return False
      checkType True = getSymbolicLinkStatus path >>= return . isSymbolicLink >>= orReplace
      checkContent False = return ()
      checkContent True = readSymbolicLink path >>= return . (== name) >>= orReplace >> return ()
      orReplace True = return True
      orReplace False = do removeRecursiveSafely path; orCreate False
      orCreate True = return True
      orCreate False = do createSymbolicLink name path; return False

-- Replace a file's contents, accounting for the possibility that the
-- old contents of the file may still be being read.  Apparently there
-- is a race condition in the file system so we may get one or more
-- isAlreadyBusyError exceptions before the writeFile succeeds.
replaceFile :: FilePath -> String -> IO ()
replaceFile path text =
    --tries 100 10 f	-- There is now a fix for this problem, see ghc ticket 2122.
    f
    where
      f :: IO ()
      f = removeFile path `E.catch` (\ e -> if isDoesNotExistError e then return () else ioError e) >> writeFile path text

-- Try something n times, returning the first Right or the last Left
-- if it never succeeds.  Sleep between tries.
--tries :: Int -> Int -> (IO a) -> IO (Either Exception a)
{-
tries _ 1 f = try f >>= either (return . Left) (return . Right)
tries usec count f = try f >>= either (\ (_ :: SomeException) -> usleep usec >> tries usec (count - 1) f) (return . Right)
-}
