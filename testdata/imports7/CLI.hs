{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad as List (mapM_)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.List (intercalate, isPrefixOf)
import Data.Set.Extra as Set (Set, toList)
import Language.Haskell.Modules
import Language.Haskell.Modules.ModuVerse (getNames)
import Language.Haskell.Modules.SourceDirs (getDirs)
import System.IO (hGetLine, hPutStr, hPutStrLn, stderr, stdin)

main :: IO ()
main = runImportsT (noisily cli)

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
           (["dir"],				dir args >> cli),
           (["split"],				split args >> cli),
           (["merge"],				merge args >> cli)]

unModuleName :: ModuleName -> String
unModuleName (ModuleName x) = x

verse :: MonadClean m => [String] -> m ()
verse [] =
    do modules <- getNames
       liftIO $ hPutStrLn stderr ("Usage: verse <pathormodule1> <pathormodule2> ...\n" ++
                                  "Add the module or all the modules below a directory to the moduVerse\n" ++
                                  "Currently:\n  " ++ showVerse modules)
verse args =
    do new <- mapM (liftIO . find) args
       List.mapM_ (List.mapM_ putModule) new
       modules <- getNames
       liftIO (hPutStrLn stderr $ "moduVerse updated:\n  " ++ showVerse modules)
    where
      find s =
          do ms <- liftIO (findHsModules [s])
             case ms of
               [] -> return [ModuleName s]
               _ -> return ms

showVerse :: Set ModuleName -> String
showVerse modules = "[ " ++ intercalate "\n  , " (map unModuleName (toList modules)) ++ " ]"

dir :: MonadClean m => [FilePath] -> m ()
dir [] = putDirs []
dir xs =
    do modifyDirs (++ xs)
       xs' <- getDirs
       liftIO (hPutStrLn stderr $ "sourceDirs updated:\n  [ " ++ intercalate "\n  , " xs' ++ " ]")

clean :: MonadClean m => [FilePath] -> m ()
clean [] = liftIO $ hPutStrLn stderr "Usage: clean <modulepath1> <modulepath2> ..."
clean args = cleanImports args >> return ()

split :: MonadClean m => [FilePath] -> m ()
split [arg] = splitModuleDecls arg >> return ()
split _ = liftIO $ hPutStrLn stderr "Usage: split <modulepath>"

merge :: MonadClean m => [String] -> m ()
merge args =
    case splitAt (length args - 1) args of
      (inputs, [output]) -> mergeModules (map ModuleName inputs) (ModuleName output) >> return ()
      _ -> liftIO $ hPutStrLn stderr "Usage: merge <inputmodulename1> <inputmodulename2> ... <outputmodulename>"
