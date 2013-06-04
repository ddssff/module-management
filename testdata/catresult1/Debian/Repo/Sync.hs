module Debian.Repo.Sync
    ( rsync
    ) where

import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString as B (empty)
import System.Exit (ExitCode)
import System.FilePath (dropTrailingPathSeparator)
import System.Process (proc)
import System.Process.Progress (keepResult, runProcessF)

rsync :: (Functor m, MonadIO m) => [String] -> FilePath -> FilePath -> m ExitCode
rsync extra source dest =
    do result <- runProcessF (proc "rsync" (["-aHxSpDt", "--delete"] ++ extra ++
                                            [dropTrailingPathSeparator source ++ "/",
                                             dropTrailingPathSeparator dest])) B.empty >>= return . keepResult
       case result of
         [x] -> return x
         _ -> error "Missing or multiple exit codes"

{-
handleExit 1 = "Syntax or usage error"
handleExit 2 = "Protocol incompatibility"
handleExit 3 = "Errors selecting input/output files, dirs"
handleExit 4 = "Requested action not supported: an attempt was made to manipulate 64-bit files on a platform that cannot support them; or an option was specified that is supported by the client and not by the server."
handleExit 5 = "Error starting client-server protocol"
handleExit 6 = "Daemon unable to append to log-file"
handleExit 10 = "Error in socket I/O"
handleExit 11 = "Error in file I/O"
handleExit 12 = "Error in rsync protocol data stream"
handleExit 13 = "Errors with program diagnostics"
handleExit 14 = "Error in IPC code"
handleExit 20 = "Received SIGUSR1 or SIGINT"
handleExit 21 = "Some error returned by waitpid()"
handleExit 22 = "Error allocating core memory buffers"
handleExit 23 = "Partial transfer due to error"
handleExit 24 = "Partial transfer due to vanished source files"
handleExit 25 = "The --max-delete limit stopped deletions"
handleExit 30 = "Timeout in data send/receive"
handleExit 35 = "Timeout waiting for daemon connection"
-}
