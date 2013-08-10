{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns, PatternGuards #-}
module Main where

import System.Environment
import System.Directory
import CLI.HaskelineTransAdapter ()
import Control.Monad as List (mapM_)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(..))
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (maybeToList)
import Data.Char (toLower)
import qualified Data.Set as S (member, toList, map)
import Data.Set.Extra as Set (Set, toList)
import Language.Haskell.Modules (cleanImports, CleanT, findHsModules, mergeModules, modifyDirs, ModuleName(..), MonadClean, noisily, putDirs, putModule, runCleanT, splitModuleDecls)
import Language.Haskell.Modules.ModuVerse (getNames)
import Language.Haskell.Modules.SourceDirs (getDirs)
import Language.Haskell.Modules.Util.QIO (modifyVerbosity)
import System.Console.Haskeline (completeFilename, CompletionFunc, defaultSettings, getInputLine, InputT, noCompletion, runInputT, setComplete, simpleCompletion)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- mapM canonicalizePath =<< getArgs
    runCleanT $ noisily $ runInputT (setComplete compl defaultSettings) $ do
        lift (verse args)
        cli


-- | these versions of quietly and noisily play well with 'lift'
quietly', noisily' :: (Monad (t m), MonadTrans t, MonadClean m) => t m b -> t m b
quietly' act = do
    lift (modifyVerbosity (\x -> x - 1))
    r <- act
    lift (modifyVerbosity (+ 1))
    return r

noisily' act = do
    lift (modifyVerbosity (+ 1))
    r <- act
    lift (modifyVerbosity (\x -> x - 1))
    return r

compl ::  CompletionFunc (CleanT IO)
compl (xs,ys) | cmd: _ <- words (reverse xs),
    matchingCommands <- filter (cmd `isPrefixOf`) commandNames = case matchingCommands of
        _:_:_ | Nothing <- stripPrefix " " =<< stripPrefix cmd (reverse xs) ->
                return ("", map simpleCompletion matchingCommands)
        [] -> return ("", map simpleCompletion commandNames)

        [x] | cmd `notElem` commandNames -> return ("", [simpleCompletion x])
            | takesModuleNames cmd -> do
                ns <- getNames
                let complAllModules = map (simpleCompletion . moduleNameToStr) (S.toList ns)
                    nsLower = S.map (ModuleName . map toLower . moduleNameToStr) ns
                    transform
                        | nsLower == ns = map toLower
                        | otherwise = id
                return $ case span (/=' ') xs of
                    (reverse -> cw @ (_:_), rest) ->
                        case filter (isPrefixOf (transform cw) . transform)
                                        (moduleNameToStr `map` S.toList ns) of
                            -- this first case isn't really needed: it should do the same as the
                            -- next case but possibly be more efficient
                            _ | ModuleName cw `S.member` ns -> (rest, [simpleCompletion cw])
                            [modName] -> (rest, [simpleCompletion modName])
                            _ -> (rest, complAllModules)
                    _ -> (xs, complAllModules)
            | otherwise  -> completeFilename (xs,ys)
        _ -> noCompletion (xs,ys)
compl x = noCompletion x

moduleNameToStr :: ModuleName -> String
moduleNameToStr (ModuleName x) = x

takesModuleNames :: String -> Bool
takesModuleNames x = x `elem` ["merge"]


cli :: InputT (CleanT IO) ()
cli = cmd . concatMap words . maybeToList =<< getInputLine "> "
  where

  cmd :: [String] -> InputT (CleanT IO) ()
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
        cmds = cmds_ args cli

commandNames :: [String]
commandNames = concatMap fst (cmds_ undefined cli)

cmds_ args next =
          [(["quit", "exit", "bye", "."],	liftIO (hPutStrLn stderr "Exiting")),
           (["v"], do
                liftIO (hPutStrLn stderr "Increasing verbosity level")
                noisily' next),
           (["q"], do
                liftIO (hPutStrLn stderr "Decreasing verbosity level")
                quietly' next),
           (["help"],				liftIO (hPutStrLn stderr "help text") >> next),
           (["verse"],				lift(verse args) >> next),
           (["clean"],				lift(clean args) >> next),
           (["dir"],				lift(dir args) >> next),
           (["split"],				lift(split args) >> next),
           (["merge"],				lift(merge args) >> next)]

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
