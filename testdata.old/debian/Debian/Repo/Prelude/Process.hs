{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Repo.Prelude.Process
    ( readProcLazy
    , readProcLazy'
    , throwProcessResult'
    , throwProcessResult''
    , throwProcessFailure
    , mapResultM
    , testExit
    , processException
    , readProcFailing
    , insertProcessEnv
    , modifyProcessEnv
    ) where

import Control.Arrow (second)
import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString.Lazy as L (ByteString)
import Data.String (IsString)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Posix.Env (setEnv, getEnv, unsetEnv)

import Control.Exception (evaluate)
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import Debian.Repo.Prelude.Verbosity
import Control.Exception (throw)
import GHC.IO.Exception (IOErrorType(OtherError))
import Language.Haskell.TH (Loc)
import Language.Haskell.TH.Syntax (Lift)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO.Error (mkIOError)
import System.Process (CreateProcess(cmdspec, cwd, env))
import System.Process.Chunks (Chunk(..), putMappedChunks, showCmdSpecForUser, readCreateProcessChunks)
import System.Process.ListLike (ListLikeLazyIO)

-- | Verbosity enabled process reader.  (Why MonadIO and not IO?)
readProcLazy :: MonadIO m => Loc -> CreateProcess -> L.ByteString -> m [Chunk L.ByteString]
readProcLazy p input = do
  v <- verbosity
  case v of
    n | n <= 0 -> liftIO $ readCreateProcessChunks p input
    1 -> liftIO $ readCreateProcessChunks p input >>= putMappedChunks (insertCommandStart p . filter (not . isOutput))
    _ -> liftIO $ readCreateProcessChunks p input >>= putIndentedShowCommand p " 1> " " 1> "

putIndentedShowCommand :: (ListLikeLazyIO a c, Eq c, IsString a) =>
                          CreateProcess -> String -> String -> [Chunk a] -> IO [Chunk a]
putIndentedShowCommand p outp errp chunks =
    putMappedChunks (insertCommandDisplay p . indentChunks outp errp) chunks

-- | Insert a chunk displaying the command and its arguments at the
-- beginning of the chunk list.
insertCommandStart :: (IsString a, ListLikeLazyIO a c, Eq c) =>
                      CreateProcess -> [Chunk a] -> [Chunk a]
insertCommandStart p chunks = [Stderr (fromString (" -> " ++ showCreateProcessForUser p ++ "\n"))] <> chunks

-- | Insert a chunk displaying the command and the result code.
insertCommandResult :: (IsString a, ListLikeLazyIO a c, Eq c) =>
                       CreateProcess -> [Chunk a] -> [Chunk a]
insertCommandResult _ [] = []
insertCommandResult p (Result code : xs) =
    Stderr (fromString (" <- " ++ show code ++ " <- " ++ showCmdSpecForUser (cmdspec p) ++ "\n")) :
    Result code :
    xs
insertCommandResult p (x : xs) = x : insertCommandResult p xs

insertCommandDisplay :: (IsString a, ListLikeLazyIO a c, Eq c) => CreateProcess -> [Chunk a] -> [Chunk a]
insertCommandDisplay p = insertCommandResult p . insertCommandStart p

-- | Verbosity enabled process reader.  (Why MonadIO and not IO?)
readProcLazy' :: (ListLikeLazyIO a c, IsString a, Eq c, MonadIO m) => CreateProcess -> a -> m [Chunk a]
readProcLazy' p input = liftIO $ do
  v <- verbosity
  case v of
    n | n <= 0 -> readCreateProcessChunks p input
    1 -> readCreateProcessChunks p input >>= putMappedChunks (insertCommandStart p . filter (not . isOutput))
    _ -> readCreateProcessChunks p input >>= putIndentedShowCommand p " 1> " " 1> "

-- | Copied from "System.Process", the exception thrown when the
-- process started by 'System.Process.readProcess' gets an
-- 'ExitFailure'.
processException :: CreateProcess -> ExitCode -> IOError
processException p code =
    mkIOError OtherError (showCmdSpecForUser (cmdspec p) ++ maybe "" (\ d -> "(in " ++ show d ++ ")") (cwd p) ++ " -> " ++ show code) Nothing Nothing

-- | Verbosity enabled process reader that throws an exception on ExitFailure.
readProcFailing :: MonadIO m => CreateProcess -> L.ByteString -> m [Chunk L.ByteString]
readProcFailing p input = readProcLazy p input >>= liftIO . throwProcessFailure p

-- | Set an environment variable in the CreateProcess, initializing it
-- with what is in the current environment.
insertProcessEnv :: [(String, String)] -> CreateProcess -> IO CreateProcess
insertProcessEnv pairs = modifyProcessEnv (map (second Just) pairs)

instance Lift Loc where
    lift x = recConE 'Loc [ (,) <$> (pure 'loc_filename) <*> litE (stringL (loc_filename x))
                          , (,) <$> (pure 'loc_package) <*> litE (stringL (loc_package x))
                          , (,) <$> (pure 'loc_module) <*> litE (stringL (loc_module x))
                          , (,) <$> (pure 'loc_start) <*> [|($(litE (integerL (fromIntegral (fst (loc_start x))))),
                                                             $(litE (integerL (fromIntegral (snd (loc_start x)))))) :: (Int, Int)|]
                          , (,) <$> (pure 'loc_end) <*> [|($(litE (integerL (fromIntegral (fst (loc_end x))))),
                                                           $(litE (integerL (fromIntegral (snd (loc_end x)))))) :: (Int, Int)|] ]

-- | Embed an expression of type Loc containing the location
-- information for the place where it appears.  Could be used in
-- custom Exception types and similar:
--
-- > throw $ MyException $LOC
__LOC__ :: Q Exp
__LOC__ = lift =<< location

throwProcessResult' :: (ExitCode -> Maybe IOError) -> CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessResult' f p chunks = mapResultM (\ code -> maybe (return $ Result code) (throw $ processException p code) (f code)) chunks

throwProcessResult'' :: Exception e => (CreateProcess -> ExitCode -> Maybe e) -> CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessResult'' f p chunks = mapResultM (\ code -> maybe (return $ Result code) throw (f p code)) chunks

throwProcessFailure :: CreateProcess -> [Chunk a] -> IO [Chunk a]
throwProcessFailure p = throwProcessResult' (testExit Nothing (Just . processException p . ExitFailure)) p

testExit :: a -> (Int -> a) -> ExitCode -> a
testExit s _ ExitSuccess = s
testExit _ f (ExitFailure n) = f n

modifyProcessEnv :: [(String, Maybe String)] -> CreateProcess -> IO CreateProcess
modifyProcessEnv pairs p = do
  env0 <- maybe getEnvironment return (env p)
  let env' = foldl modEnv1 env0 pairs
  return $ p {env = Just env'}

modEnv1 :: [(String, String)] -> (String, Maybe String) -> [(String, String)]
modEnv1 env0 (name, mvalue) = maybe [] (\ v -> [(name, v)]) mvalue ++ filter ((/= name) . fst) env0
