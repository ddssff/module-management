#!/usr/bin/env runghc

module Main where

import Options.Applicative
import Control.Monad.Trans (MonadIO(liftIO))
import Language.Haskell.Modules (cleanImports, runCleanT)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

main = do
  args <- getArgs
  fs <- if null args then sourceFiles "." else return args
  cs <- runCleanT $ mapM cleanImports' fs
  print cs

cleanImports' fp = do (liftIO $ putStr "cleaning: " >> print fp) >> cleanImports fp

sourceFiles :: FilePath -> IO [FilePath]
sourceFiles fp = getDirectoryContents fp >>= return . filter ((".hs" ==) . takeExtension)
