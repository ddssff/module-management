{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad.Trans (MonadIO(liftIO))
import Data.List (intercalate, isPrefixOf)
import Data.Set (fromList, union)
import Language.Haskell.Exts.Syntax (ModuleName(ModuleName))
import Language.Haskell.Modules (runMonadClean, cleanImports, splitModule, mergeModules, modifyModuVerse)
import Language.Haskell.Modules.Params (MonadClean)
import Language.Haskell.Modules.Util.QIO (noisily, quietly)
import System.IO (stdin, stderr, hGetLine, hPutStr, hPutStrLn)

main :: IO ()
main = runMonadClean (noisily cli)

cli :: MonadClean m => m ()
cli = liftIO (hPutStr stderr " > " >> hGetLine stdin) >>= cmd . words

cmd :: MonadClean m => [String] -> m ()
cmd [] = cli
cmd (s : args) =
    case filter (any (== s) . fst) cmds of
      [(_, f)] -> f
      -- No exact matches - look for prefix matches
      [] -> case filter (any (isPrefixOf s) . fst) cmds of
              [(_, f)] -> f
              [] -> liftIO (hPutStrLn stderr $
                              show s ++ " invalid - expected " ++ intercalate ", " (concatMap fst cmds)) >> cli
              xs -> liftIO (hPutStrLn stderr $
                              show s ++ " ambiguous - expected " ++ intercalate ", " (concatMap fst xs)) >> cli
      _ -> error $ "Internal error - multiple definitions for " ++ show s
    where
      cmds =
          [(["quit", "exit", "bye", "."],	liftIO (hPutStrLn stderr "Exiting")),
           (["v"],				liftIO (hPutStrLn stderr "Increasing verbosity level") >> noisily cli),
           (["q"],				liftIO (hPutStrLn stderr "Decreasing verbosity level") >> quietly cli),
           (["help"],				liftIO (hPutStrLn stderr "help text") >> cli),
           (["verse"],				verse args >> cli),
           (["clean"],				clean args >> cli),
           (["split"],				split args >> cli),
           (["merge"],				merge args >> cli)]

verse :: MonadClean m => [String] -> m ()
verse [] = liftIO $ hPutStrLn stderr "Usage: verse <modulename1> <modulename2> ..."
verse args =
    do modifyModuVerse (union (fromList (map ModuleName args)))
       liftIO (hPutStrLn stderr "moduVerse updated")

clean :: MonadClean m => [FilePath] -> m ()
clean [] = liftIO $ hPutStrLn stderr "Usage: clean <modulepath1> <modulepath2> ..."
clean args = mapM_ cleanImports args

split :: MonadClean m => [String] -> m ()
split [arg] = splitModule (ModuleName arg)
split _ = liftIO $ hPutStrLn stderr "Usage: split <modulename>"

merge :: MonadClean m => [String] -> m ()
merge args =
    case splitAt (length args - 1) args of
      (inputs, [output]) -> mergeModules (map ModuleName inputs) (ModuleName output) >> return ()
      _ -> liftIO $ hPutStrLn stderr "Usage: merge <inputmodulename1> <inputmodulename2> ... <outputmodulename>"
