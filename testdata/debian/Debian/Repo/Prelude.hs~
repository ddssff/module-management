{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings,
             PackageImports, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Repo.Prelude
    ( countTasks
    , nub'
    , access
    , (~=)
    , (%=)
    , symbol
    , Debian.Repo.Prelude.Verbosity.readProcFailing -- re-export
    , rsync
    , partitionM
    , maybeWriteFile
    , replaceFile
    , cond
    , listIntersection
    , sameInode
    , sameMd5sum
    , isSublistOf
    , cd
    , cartesianProduct
    , writeFileIfMissing
    , getSubDirectories
    , dropPrefix
    ) where

import Control.Exception (Exception)
import Control.Monad.State (get, modify, MonadIO, MonadState)
import Control.Monad.Trans (liftIO)
import Data.Lens.Lazy (getL, Lens, modL)
import Data.List (group, sort)
import Data.List as List (map)
import Data.Monoid (mempty)
import Data.Typeable (Typeable)
import Debian.Repo.Prelude.Bool (cond)
import Debian.Repo.Prelude.Files (getSubDirectories, maybeWriteFile, replaceFile, writeFileIfMissing)
import Debian.Repo.Prelude.GPGSign (cd)
import Debian.Repo.Prelude.List (cartesianProduct, dropPrefix, isSublistOf, listIntersection, partitionM)
import Debian.Repo.Prelude.Misc (sameInode, sameMd5sum)
import Debian.Repo.Prelude.Verbosity (ePutStrLn, readProcLazy', throwProcessResult'', readProcFailing)
import Language.Haskell.TH (Exp(LitE), Lit(StringL), Name, nameBase, nameModule, Q)
import System.Exit (ExitCode(..))
import System.FilePath (dropTrailingPathSeparator)
import System.Process (CreateProcess, proc)
import System.Process.Chunks (collectProcessTriple)
import System.Process.ListLike.LazyString ()
import Text.Printf (printf)

-- | Perform a list of tasks with log messages.
countTasks :: MonadIO m => [(String, m a)] -> m [a]
countTasks tasks =
    mapM (countTask (length tasks)) (zip [1..] tasks)
    where
      countTask :: MonadIO m => Int -> (Int, (String, m a)) -> m a
      countTask count (index, (message, task)) =
          ePutStrLn (printf "[%2d of %2d] %s:" index count message) >> task

-- | This nub doesn't preserve order
nub' :: (Ord a) => [a] -> [a]
nub' = List.map head . group . sort

access :: MonadState a m => Lens a b -> m b
access l = get >>= return . getL l

(~=) :: MonadState a m => Lens a b -> b -> m ()
l ~= x = l %= const x

-- | Modify a value.  (This is a version of Data.Lens.Lazy.%= that returns () instead of a.)
(%=) :: MonadState a m => Lens a b -> (b -> b) -> m ()
l %= f = modify (modL l f)

-- | Build a string containing a symbol's fully qualified name (for debugging output.)
symbol :: Name -> Q Exp
symbol x = return $ LitE (StringL (maybe "" (++ ".") (nameModule x) ++ nameBase x))

#if 0
-- | Convenience function for running shell commands
--    1. Runs quietly by default, but that can be controlled by the VERBOSITY environement variable
--    2. If it exits with an error code all its output is echoed (with prefixes) and an exception is thrown
--    3. The process input is the empty string
--    4. When the output is echoed stdout is prefixed with "1>" and stderr with "2>".
--    5. The output is a stream of 'Chunk'
runProc :: MonadIO m => CreateProcess -> m [Chunk B.ByteString]
-- runProc p = quieter 1 $ runProcessF prefixes p L.empty
runProc p = liftIO $ readProcessChunks p L.empty >>= putIndented nl " 1> " " 2> " >>= mapM doChunk
    where
      nl :: Word8
      nl = fromIntegral (ord '\n')
      doChunk :: Chunk B.ByteString -> IO (Chunk B.ByteString)
      doChunk (Result f@(ExitFailure _)) = error (showCmdSpecForUser (cmdspec p) ++ " -> " ++ show f)
      doChunk x = return x
      indent :: T.Text -> T.Text -> [T.Text]
      indent pre text = map (pre <>) (T.lines text)

-- | Like runProc, but does not raise an exception when process exit code is not 0.
readProc :: MonadIO m => CreateProcess -> m [Chunk B.ByteString]
readProc p = quieter 1 $ runProcess p L.empty
#endif

rsync :: forall m. (Functor m, MonadIO m) => [String] -> FilePath -> FilePath -> m (ExitCode, String, String)
rsync extra source dest =
    do let p = proc "rsync" (["-aHxSpDt", "--delete"] ++ extra ++
                             [dropTrailingPathSeparator source ++ "/",
                              dropTrailingPathSeparator dest])
       readProcLazy' p (mempty :: String) >>= liftIO . throwProcessResult'' throwRsyncError p >>= return . collectProcessTriple

#if 1
instance Exception RsyncError

data RsyncError
    = RsyncSuccess
    | RsyncSyntaxOrUsage
    | RsyncProtocolIncompatibility
    | RsyncErrorsSelectingInputOutputFiles
    | RsyncRequestedActionNotSupported
    | RsyncErrorStartingClientServerProtocol
    | RsyncDaemonUnableToAppendToLogfile
    | RsyncErrorInSocketIO
    | RsyncErrorInFileIO
    | RsyncErrorInRsyncProtocolDataStream
    | RsyncErrorsWithProgramDiagnostics
    | RsyncErrorInIPCCode
    | RsyncReceivedSIGUSR1orSIGINT
    | RsyncSomeErrorReturnedByWaitpid
    | RsyncErrorAllocatingCoreMemoryBuffers
    | PartialTransferDueToError
    | PartialTransferDueToVanishedSourceFiles
    | TheMaxDeleteLimitStoppedDeletions
    | TimeoutInDataSendReceive
    | TimeoutWaitingForDaemonConnection
    | RsyncUnexpected Int
    deriving (Typeable, Show)

rsyncError :: Int -> RsyncError
rsyncError = fst . rsyncErrorInfo

rsyncErrorMessage :: Int -> String
rsyncErrorMessage = snd . rsyncErrorInfo

rsyncErrorInfo :: Int -> (RsyncError, String)
rsyncErrorInfo 1  = (RsyncSyntaxOrUsage, "rsync: Syntax or usage error")
rsyncErrorInfo 2  = (RsyncProtocolIncompatibility, "rsync: Protocol incompatibility")
rsyncErrorInfo 3  = (RsyncErrorsSelectingInputOutputFiles, "rsync: Errors selecting input/output files, dirs")
rsyncErrorInfo 4  = (RsyncRequestedActionNotSupported, "rsync: Requested action not supported: an attempt was made to manipulate 64-bit files on a platform that cannot support them; or an option was specified that is supported by the client and not by the server.")
rsyncErrorInfo 5  = (RsyncErrorStartingClientServerProtocol, "rsync: Error starting client-server protocol")
rsyncErrorInfo 6  = (RsyncDaemonUnableToAppendToLogfile, "rsync: Daemon unable to append to log-file")
rsyncErrorInfo 10 = (RsyncErrorInSocketIO, "rsync: Error in socket I/O")
rsyncErrorInfo 11 = (RsyncErrorInFileIO, "rsync: Error in file I/O")
rsyncErrorInfo 12 = (RsyncErrorInRsyncProtocolDataStream, "rsync: Error in rsync protocol data stream")
rsyncErrorInfo 13 = (RsyncErrorsWithProgramDiagnostics, "rsync: Errors with program diagnostics")
rsyncErrorInfo 14 = (RsyncErrorInIPCCode, "rsync: Error in IPC code")
rsyncErrorInfo 20 = (RsyncReceivedSIGUSR1orSIGINT, "rsync: Received SIGUSR1 or SIGINT")
rsyncErrorInfo 21 = (RsyncSomeErrorReturnedByWaitpid, "rsync: Some error returned by waitpid()")
rsyncErrorInfo 22 = (RsyncErrorAllocatingCoreMemoryBuffers, "rsync: Error allocating core memory buffers")
rsyncErrorInfo 23 = (PartialTransferDueToError, "Partial transfer due to error")
rsyncErrorInfo 24 = (PartialTransferDueToVanishedSourceFiles, "Partial transfer due to vanished source files")
rsyncErrorInfo 25 = (TheMaxDeleteLimitStoppedDeletions, "rsync: The --max-delete limit stopped deletions")
rsyncErrorInfo 30 = (TimeoutInDataSendReceive, "rsync: Timeout in data send/receive")
rsyncErrorInfo 35 = (TimeoutWaitingForDaemonConnection, "rsync: Timeout waiting for daemon connection")
rsyncErrorInfo n = (RsyncUnexpected n, "Unexpected rsync error: " ++ show n)
#endif

throwRsyncError :: CreateProcess -> ExitCode -> Maybe RsyncError
throwRsyncError _ ExitSuccess = Nothing
throwRsyncError _ (ExitFailure n) = Just $ fst $ rsyncErrorInfo n
#if 0
    case n of
      1 -> error "rsync: Syntax or usage error"
      2 -> error "rsync: Protocol incompatibility"
      3 -> error "rsync: Errors selecting input/output files, dirs"
      4 -> error "rsync: Requested action not supported: an attempt was made to manipulate 64-bit files on a platform that cannot support them; or an option was specified that is supported by the client and not by the server."
      5 -> error "rsync: Error starting client-server protocol"
      6 -> error "rsync: Daemon unable to append to log-file"
      10 -> error "rsync: Error in socket I/O"
      11 -> error "rsync: Error in file I/O"
      12 -> error "rsync: Error in rsync protocol data stream"
      13 -> error "rsync: Errors with program diagnostics"
      14 -> error "rsync: Error in IPC code"
      20 -> error "rsync: Received SIGUSR1 or SIGINT"
      21 -> error "rsync: Some error returned by waitpid()"
      22 -> error "rsync: Error allocating core memory buffers"
      23 -> error "Partial transfer due to error"
      24 -> error "Partial transfer due to vanished source files"
      25 -> error "rsync: The --max-delete limit stopped deletions"
      30 -> error "rsync: Timeout in data send/receive"
      35 -> error "rsync: Timeout waiting for daemon connection"
      _ -> error $ "rsync: Unexpected failure " ++ show n
#endif
