{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
-- | This module, except for useEnv, is copied from the build-env package.
module Debian.Repo.Prelude.Chroot
    ( fchroot
    , useEnv
    -- , forceList  -- moved to progress
    -- , forceList'
    ) where

import Control.Exception (finally, evaluate)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Debian.Repo.Prelude.Verbosity (readProc)
import Foreign.C.Error
import Foreign.C.String
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (dropTrailingPathSeparator, dropFileName)
import System.IO (hPutStr, stderr)
import System.IO.Error (catchIOError)
import System.Posix.Env (getEnv)
import System.Posix.IO
import System.Posix.Directory
import System.Process (readProcess, readProcessWithExitCode, showCommandForUser, proc)

foreign import ccall unsafe "chroot" c_chroot :: CString -> IO Int

{-# DEPRECATED forceList "If you need forceList enable it in progress-System.Unix.Process." #-}
forceList = undefined
{-# DEPRECATED forceList' "If you need forceList' enable it in progress-System.Unix.Process." #-}
forceList' = undefined

-- |chroot changes the root directory to filepath
-- NOTE: it does not change the working directory, just the root directory
-- NOTE: will throw IOError if chroot fails
chroot :: FilePath -> IO ()
chroot fp = withCString fp $ \cfp -> throwErrnoIfMinus1_ "chroot" (c_chroot cfp)

-- |fchroot runs an IO action inside a chroot
-- fchroot performs a chroot, runs the action, and then restores the
-- original root and working directory. This probably affects the
-- chroot and working directory of all the threads in the process,
-- so...
-- NOTE: will throw IOError if internal chroot fails
fchroot :: FilePath -> IO a -> IO a
fchroot path action =
    do origWd <- getWorkingDirectory
       rootFd <- openFd "/" ReadOnly Nothing defaultFileFlags
       chroot path
       changeWorkingDirectory "/"
       action `finally` (breakFree origWd rootFd)
    where
      breakFree origWd rootFd =
          do changeWorkingDirectoryFd rootFd
             closeFd rootFd
             chroot "."
             changeWorkingDirectory origWd

-- | The ssh inside of the chroot needs to be able to talk to the
-- running ssh-agent.  Therefore we mount --bind the ssh agent socket
-- dir inside the chroot (and umount it when we exit the chroot.
useEnv :: FilePath -> (a -> IO a) -> IO a -> IO a
useEnv rootPath force action =
    do -- In order to minimize confusion, this QIO message is output
       -- at default quietness.  If you want to suppress it while seeing
       -- the output from your action, you need to say something like
       -- quieter (+ 1) (useEnv (quieter (\x->x-1) action))
       sockPath <- getEnv "SSH_AUTH_SOCK"
       home <- getEnv "HOME"
       copySSH home
       -- We need to force the output before we exit the changeroot.
       -- Otherwise we lose our ability to communicate with the ssh
       -- agent and we get errors.
       withSock sockPath . fchroot rootPath $ (action >>= force)
    where
      copySSH Nothing = return ()
      copySSH (Just home) = do
        -- Do NOT preserve ownership, files must be owned by root.
        createDirectoryIfMissing True (rootPath ++ "/root")
        readProc (proc "/usr/bin/rsync" ["-rlptgDHxS", "--delete", home ++ "/.ssh/", rootPath ++ "/root/.ssh"]) "" `catchIOError` (\ e -> error $ "Failure in useEnv:copySSH - " ++ show e)
        return ()

      withSock Nothing action = action
      withSock (Just sockPath) action =
          withMountBind dir (rootPath ++ dir) action
          where dir = dropTrailingPathSeparator (dropFileName sockPath)
      withMountBind toMount mountPoint action =
          (do createDirectoryIfMissing True mountPoint
              readProc (proc "/bin/mount" ["--bind", toMount, mountPoint]) "" `catchIOError` (\ e -> error $ "Failure mounting in useEnv:withMountBind - " ++ show e)
              action) `finally` do readProc (proc "/bin/umount" [mountPoint]) "" `catchIOError` (\ e -> error $ "Failure unmounting in useEnv:withMountBind - " ++ show e)
                                   return ()

{-
      run cmd args =
          do (code, out, err) <- readProcessWithExitCode cmd args ""
             case code of
               ExitSuccess -> return ()
               _ -> error ("Exception in System.Unix.Chroot.useEnv: " ++ showCommandForUser cmd args ++ " -> " ++ show code ++
                           "\n\nstdout:\n " ++ prefix "> " out ++ "\n\nstderr:\n" ++ prefix "> " err)
-}
      prefix pre s = unlines (map (pre ++) (lines s))

{-
printDots :: Int -> [Output] -> IO [Output]
printDots cpd output =
    foldM f 0 output >> return output
    where
      print rem (Stdout s) =
          let (dots, rem') = quotRem (rem + length s) in
          hPutStr stderr (replicate dots '.')
          return rem'
      print rem (Stderr s) = print rem (Stdout s)
-}
