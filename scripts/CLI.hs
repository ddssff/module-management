{- # OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, PatternGuards, RankNTypes, StandaloneDeriving, ViewPatterns #-}
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
import Distribution.Package (InstalledPackageId(..))
import GHC.Generics (Generic)
import qualified Language.Haskell.Exts.Annotated as A (Decl)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..))
import Language.Haskell.Exts.Syntax (Name(Ident, Symbol))
import Language.Haskell.Modules (cleanImports, CleanT, findHsModules, mergeModules,
                                 modifyHsSourceDirs, ModuleName(..), MonadClean, noisily, putHsSourceDirs, putModule,
                                 runImportsT, splitModuleBy)
import Language.Haskell.Modules.ModuVerse (getNames, getInfo, moduleName)
import Language.Haskell.Modules.Params (CleanMode(DoClean))
import Language.Haskell.Modules.SourceDirs (getHsSourceDirs)
import Language.Haskell.Modules.Split (T(A))
import Language.Haskell.Modules.Util.Symbols (FoldDeclared)
import Language.Haskell.Modules.Util.QIO (modifyVerbosity)
import Language.Haskell.TH.Syntax as TH (nameBase)
import System.Console.Haskeline (completeFilename, CompletionFunc, defaultSettings, getInputLine, InputT, noCompletion, runInputT, setComplete, simpleCompletion)
import System.IO (hPutStrLn, stderr)
import Text.Regex (mkRegex, matchRegex)

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
  { verbosity :: Int,
    verbosityCabal :: Int,
    caseSensitiveCompletion :: Bool,
    cabalFile :: Maybe FilePath,
    otherFiles :: [FilePath] }
    deriving (Data,Typeable,Show)

defaultHMM = CLI
  { Main.verbosity = fromEnum Loud
            &= enumHelp "module-management verbosity" (undefined :: Verbosity)
                        (Main.verbosity defaultHMM),
    verbosityCabal = fromEnum (maxBound :: Distribution.Verbosity.Verbosity)
            &= enumHelp "cabal file parser verbosity"
                        (undefined :: Distribution.Verbosity.Verbosity)
                        (verbosityCabal defaultHMM),
    caseSensitiveCompletion = False
            &= help "should completion of module names be case-sensitive?"
            &= name "s",
    cabalFile = Nothing
            &= typFile
            &= name "f"
            &= help (concat
                     [".cabal file to load. Modules listed there will be loaded",
                      " and when modules are split the .cabal file will be updated",
                      " when you quit or use the cabalWrite command"]),
    otherFiles = []
            &= typFile
            &= args }


enumHelp item proxy def =
    help ("Level of "++item++". Takes values from " ++
            rangeOfBounded (undefined `asTypeOf` proxy) ++
            ". Default is "++ show def ++ ".")
rangeOfBounded proxy = show (fromEnum (minBound `asTypeOf` proxy))
    ++ " to "
    ++ show (fromEnum (maxBound `asTypeOf` proxy))


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
    let args = otherFiles conf

    pkgDesc' <-
        traverse (Cabal.readPackageDescription (toEnumBounded (verbosityCabal conf)))
            (cabalFile conf)


    let initState = do
            traverse (dir . (".":) . Cabal.getSrcDirs) pkgDesc'
            modifyVerbosity $ \ _ -> Main.verbosity conf
            let modules0 = args ++ (map moduleNameToStr $ Cabal.getModules =<< maybeToList pkgDesc')
            when (not (null modules0)) $ verse modules0


        execCmdM :: CmdM a -> IO (Maybe GenericPackageDescription)
        execCmdM x = runImportsT
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
      case filter (any (== s) . names) cmds of
        [x] -> action x
        -- No exact matches - look for prefix matches
        [] -> case filter (any (isPrefixOf s) . names) cmds of
                [x] -> action x
                [] -> liftIO (hPutStrLn stderr $
                                show s ++ " invalid - expected " ++ intercalate ", " (concatMap names cmds)) >> cli
                xs -> liftIO (hPutStrLn stderr $
                                show s ++ " ambiguous - expected " ++ intercalate ", " (concatMap names xs) ++ "\n\n" ) >> cli
        _ -> error $ "Internal error - multiple definitions for " ++ show s
        where
        cmds = cmds_ args


data Callback = Callback String (CmdM () -> CmdM ()) deriving (Typeable)
instance Show Callback where
    show (Callback x _) = x
instance Exception Callback

data Command
    = Command
       { names :: [String]
       , action :: CmdM ()
       , usage :: String }

commandNames :: [String]
commandNames = concatMap names (cmds_ undefined)

cmds_ :: [String] -> [Command]
cmds_ args =
          [Command { names = ["quit", "exit", "bye", ".", "\EOT"],
                     action = do liftIO (hPutStrLn stderr "Exiting")
                                 throwIO ExitSuccess,
                     usage = "quit\n" ++ "Exit the interpreter." },
           Command { names = ["v"],
                     action = throwIO . Callback "louder" $ \next -> do
                                liftIO (hPutStrLn stderr "Increasing Verbosity")
                                noisily' next,
                     usage = "v\n" ++ "Increase the verbosity of the interpreter messages" },
           Command { names = ["q"],
                     action = throwIO . Callback "quieter" $ \next -> do
                                liftIO (hPutStrLn stderr "Decreasing Verbosity")
                                quietly' next,
                     usage = "q\n" ++ "Decrease the verbosity of the interpreter messages" },
           -- The error message is more helpful
           Command { names = ["help"],
                     action = liftIO (hPutStrLn stderr helpMessage),
                     usage = "help\n" ++ "Print usage information.  Remember that this script doesn't understand shell escaping and quoting." },
           Command { names = ["verse"],
                     action = liftCT (verse args),
                     usage = "verse <pathormodule1> <pathormodule2> ...\n" ++
                             "Add the module or all the modules below a directory to the module universe.  " ++
                             "This is the set of modules whose references will be updated when a symbol " ++
                             "is moved from one module to another." },
           Command { names = ["clean"],
                     action =  liftCT (clean args),
                     usage = "clean <modulepath1> <modulepath2> ...\n" ++
                             "Clean up the import lists of the named modules" },
           Command { names = ["dir"],
                     action =    liftCT (dir args),
                     usage = "dir <directory> <directory> ...\n" ++
                             "Add paths to the list of search directories - similar to the ghc -i option." },
{-
           Command { names = ["split"],
                     action =  split args,
                     usage = "split <modulepath>\n" ++
                             "Split each of the symbols in a module into individual sub-modules.  Updates all\n" ++
                             "references to these symbols throughout the moduverse." },
-}
           Command { names = ["splitBy"],
                     action =  splitBy args,
                     usage = "splitBy <regex> <newmodule> <oldmodule>\n" ++
                             "Partition the symbols of a module into new modules based on the function\n" ++
                             "mapping the symbol name to the new module name.  For example,\n" ++
                             "split '(\\ s -> case s of \"foo\" -> \"Module.One\"; _ -> \"Module.Two\")' Module.Orig\n" ++
                             "Updates all references to these symbols throughout the moduverse." },
           Command { names = ["merge"],
                     action =  merge args,
                     usage = "merge <inputmodulepath1> <inputmodulepath2> ... <outputmodulepath>\n" ++
                             "Merge the given input modules into a single output module.  Updates all\n" ++
                             "references to these symbols throughout the moduverse." },
           Command { names = ["cabalPrint"],
                     action =  cabalPrint,
                     usage = "" },
           Command {names = ["cabalRead"],
                     action =  cabalRead args,
                     usage = "" },
           Command {names = ["cabalWrite"],
                     action =  cabalWrite args,
                     usage = "" }]

helpMessage :: String
helpMessage = intercalate "\n\n" (map usage (cmds_ undefined))

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
        ds <- getHsSourceDirs
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
dir [] = putHsSourceDirs []
dir xs =
    do modifyHsSourceDirs (++ xs)
       xs' <- getHsSourceDirs
       liftIO (putStrLn $ "sourceDirs updated:\n  [ " ++ intercalate "\n  , " xs' ++ " ]")

clean :: MonadClean m => [FilePath] -> m ()
clean [] = liftIO $ putStrLn $ "Usage: clean <modulepath1> <modulepath2> ...\n" ++
                                    "Clean up the import lists of the named modules\n"
clean args = cleanImports args >> return ()

#if 0
split :: [String] -> CmdM ()
split [arg] = do
    r <- liftCT (splitModuleDecls DoClean arg)
    lift (modify (Cabal.update r))
    return ()
split _ = liftIO $ putStrLn "Usage: split <modulepath>"
#endif

splitBy :: [String] -> CmdM ()
splitBy [regex, newModule, oldModule] = do
  r <- liftCT (getInfo (ModuleName oldModule) >>=
               maybe (error $ "Module not found: " ++ show oldModule)
                     (\oldModuleInfo -> splitModuleBy DoClean pred oldModuleInfo))
  lift (modify (Cabal.update r))
  return ()
    where
      -- pred :: Maybe Name -> T -> ModuleName
      pred name decl =
          -- declarations not associated with symbols stay in
          -- oldModules (e.g. instances.)  Decarations of matching
          -- symbols go to newModule
          let match = name >>= (\x -> Just $ case x of Ident s -> s; Symbol s -> s) >>= matchRegex (mkRegex regex)
              modname = ModuleName (maybe oldModule (const newModule) match) in
          {- trace ("Symbol " ++ show name ++ " -> " ++ show modname ++ ", matchRegex (mkRegex " ++ show regex ++ ") -> " ++ show match)-}
                modname

splitBy _ = liftIO $ putStrLn "Usage: splitBy <regexp> <newmodule> <oldmodule>"

merge :: [String] -> CmdM ()
merge args =
    case splitAt (length args - 1) args of
      (inputs, [output]) -> do
            r <- liftCT $ mergeModules DoClean (map ModuleName inputs) (ModuleName output)
            liftS (modify (Cabal.update r))
      _ -> liftIO $ putStrLn "Usage: merge <inputmodulename1> <inputmodulename2> ... <outputmodulename>"
