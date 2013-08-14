{- # OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns, PatternGuards, DeriveDataTypeable #-}
module Main where

import System.Console.Haskeline
import Control.Monad.State

import System.Environment
import System.Directory
import CLI.HaskelineTransAdapter ()
import Control.Monad as List (mapM_)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(..))
import Data.List
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

import System.Console.CmdArgs
import Control.Lens
import Data.Maybe

import Control.Monad.Reader

import Distribution.PackageDescription (GenericPackageDescription)

import qualified CLI.Cabal as Cabal
import qualified Distribution.Verbosity
import Data.Data

import Data.Foldable(traverse_)
import System.Console.Haskeline.MonadException
import Control.Exception (fromException)

import System.Exit

data HMM = CLI
  { verbosity :: Verbosity,
    verbosityCabal :: Int,
    caseSensitiveCompletion :: Bool,
    cabalFile :: Maybe FilePath,
    otherFiles :: [FilePath] }
    deriving (Data,Typeable,Show)

defaultHMM = CLI
  { Main.verbosity = Loud,
    verbosityCabal = fromEnum (maxBound :: Distribution.Verbosity.Verbosity),
    caseSensitiveCompletion = False,
    cabalFile = Nothing,
    otherFiles = [] &= args }


toEnumBounded :: (Bounded e, Enum e) => Int -> e
toEnumBounded i =
    let r | i > iMax = maxBound
          | i < iMin = minBound
          | otherwise = toEnum i
        iMax = fromEnum (maxBound `asTypeOf` r)
        iMin = fromEnum (minBound `asTypeOf` r)
    in r

main :: IO ()
main = do
    conf <- cmdArgs defaultHMM
    args <- mapM canonicalizePath (otherFiles conf)

    pkgDesc' <-
        traverse (Cabal.readPackageDescription (toEnumBounded (verbosityCabal conf)))
            (cabalFile conf)


    let initState = do
            traverse (dir . (".":) . Cabal.getSrcDirs) pkgDesc'
            modifyVerbosity $ \ _ -> fromEnum (Main.verbosity conf)
            let modules0 = args ++ (map moduleNameToStr $ Cabal.getModules =<< maybeToList pkgDesc')
            when (not (null modules0)) $ verse modules0


        execCmdM :: CmdM a -> IO (Maybe GenericPackageDescription)
        execCmdM x = runCleanT
            $ flip execStateT pkgDesc'
            $ flip runReaderT conf
            $ runInputT (setComplete ((lift . lift) `fmap` compl conf) defaultSettings) x

    pkgDesc' <- execCmdM $ do
        liftCT initState
        let step = cli
            loop = catch (do step; loop) $ \e @ SomeException {} -> case () of
                    _ | Just e <- fromException e -> throwIO (e `asTypeOf` ExitSuccess)
                      | Just (Callback _msg f) <- fromException e -> f loop
                    _ -> do
                        liftIO (print e)
                        loop
        loop

    -- this gets bypassed when the ExitCode is re-thrown.
    -- Should it be run on ExitFailures or just success?
    traverse_ (uncurry Cabal.writeGenericPackageDescription)
        $ liftM2 (,) (cabalFile conf) pkgDesc'


-- | these versions of quietly and noisily play well with 'lift'
quietly', noisily' :: CmdM a -> CmdM a
quietly' act = do
    liftCT (modifyVerbosity (\x -> x - 1))
    r <- act
    liftCT (modifyVerbosity (+ 1))
    return r

noisily' act = do
    liftCT (modifyVerbosity (+ 1))
    r <- act
    liftCT (modifyVerbosity (\x -> x - 1))
    return r

compl ::  HMM -> CompletionFunc (CleanT IO)
compl conf (xs,ys) | cmd: _ <- words (reverse xs),
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
                        | caseSensitiveCompletion conf = id
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
compl _ x = noCompletion x

moduleNameToStr :: ModuleName -> String
moduleNameToStr (ModuleName x) = x

takesModuleNames :: String -> Bool
takesModuleNames x = x `elem` ["merge"]


cli :: CmdM ()
cli = cmd . concatMap words . maybeToList =<< getInputLine "> "

type CmdM a = InputT (ReaderT HMM (StateT (Maybe GenericPackageDescription) (CleanT IO))) a

askConf :: CmdM HMM
askConf = lift ask

liftCT :: CleanT IO a -> CmdM a
liftCT = lift . lift . lift

liftS :: StateT (Maybe GenericPackageDescription) (CleanT IO) a -> CmdM a
liftS  = lift . lift


cmd :: [String] -> CmdM ()
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
        cmds = cmds_ args


data Callback = Callback String (CmdM () -> CmdM ()) deriving (Typeable)
instance Show Callback where
    show (Callback x _) = x
instance Exception Callback

commandNames :: [String]
commandNames = concatMap fst (cmds_ undefined)

cmds_ :: [String] -> [([String], CmdM ())]
cmds_ args =
          [(["quit", "exit", "bye", ".", "\EOT"], do
                liftIO (hPutStrLn stderr "Exiting")
                throwIO ExitSuccess),
           (["v"], throwIO . Callback "louder" $ \next -> do
                    liftIO (hPutStrLn stderr "Increasing Verbosity")
                    noisily' next),
           (["q"], throwIO . Callback "quieter" $ \next -> do
                    liftIO (hPutStrLn stderr "Decreasing Verbosity")
                    quietly' next),
           (["help"],  liftIO (hPutStrLn stderr "help text")),
           (["verse"], liftCT (verse args)),
           (["clean"], liftCT (clean args)),
           (["dir"],   liftCT (dir args)),
           (["split"], split args),
           (["merge"], merge args),
           (["cabalPrint"], cabalPrint),
           (["cabalRead"], cabalRead args),
           (["cabalWrite"], cabalWrite args)]

cabalPrint :: CmdM ()
cabalPrint = do
    pkgDesc <- liftS get
    liftIO $ putStrLn $ case pkgDesc of
        Nothing -> "No cabal file loaded"
        Just x -> Cabal.showGenericPackageDescription x

cabalWrite, cabalRead :: [String] -> CmdM ()
cabalWrite [f] = do
    pkgDesc <- liftS get
    liftIO $ case pkgDesc of
        Nothing -> putStrLn "cabalWrite: no cabal file was loaded"
        Just x -> Cabal.writeGenericPackageDescription f x
cabalWrite [] = do
    conf <- askConf
    case cabalFile conf of
        Nothing -> liftIO $ putStrLn "Usage: cabalWrite <file.cabal>"
        Just f -> cabalWrite [f] -- goto previous case
cabalWrite _ = liftIO $ putStrLn "Usage: cabalWrite <file.cabal>"

cabalRead [f] = throwIO . Callback "cabalRead" $ \next -> do
    conf <- askConf
    pd <- liftIO $ Cabal.readPackageDescription (toEnumBounded (verbosityCabal conf)) f

    liftCT $ do
        ds <- getDirs
        let ds' = Cabal.getSrcDirs pd
        unless (null $ ds' \\ ds) $ dir ds'

        verse (unModuleName `map` Cabal.getModules pd)

    mapInputT (local (\x -> x{ cabalFile = Just f })) next

cabalRead _ = liftIO $ putStrLn "Usage: cabalRead <file.cabal>"

unModuleName :: ModuleName -> String
unModuleName (ModuleName x) = x

verse :: MonadClean m => [String] -> m ()
verse [] =
    do modules <- getNames
       liftIO $ putStrLn ("Usage: verse <pathormodule1> <pathormodule2> ...\n" ++
                                  "Add the module or all the modules below a directory to the moduVerse\n" ++
                                  "Currently:\n  " ++ showVerse modules)
verse args =
    do new <- mapM (liftIO . find) args
       List.mapM_ (List.mapM_ putModule) new
       modules <- getNames
       liftIO (putStrLn $ "moduVerse updated:\n  " ++ showVerse modules)
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
       liftIO (putStrLn $ "sourceDirs updated:\n  [ " ++ intercalate "\n  , " xs' ++ " ]")

clean :: MonadClean m => [FilePath] -> m ()
clean [] = liftIO $ putStrLn "Usage: clean <modulepath1> <modulepath2> ..."
clean args = cleanImports args >> return ()

split :: [FilePath] -> CmdM ()
split [arg] = do
    r <- liftCT (splitModuleDecls arg)
    lift (modify (Cabal.update r))
    return ()
split _ = liftIO $ putStrLn "Usage: split <modulepath>"

merge :: [String] -> CmdM ()
merge args =
    case splitAt (length args - 1) args of
      (inputs, [output]) -> do
            r <- liftCT $ mergeModules (map ModuleName inputs) (ModuleName output)
            liftS (modify (Cabal.update r))
      _ -> liftIO $ putStrLn "Usage: merge <inputmodulename1> <inputmodulename2> ... <outputmodulename>"
