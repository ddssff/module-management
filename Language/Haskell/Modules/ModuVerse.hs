{-# LANGUAGE CPP, FlexibleInstances, PackageImports, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.ModuVerse
    ( ModuleInfo
    , moduleName
    , ModuVerseState
    , moduVerseInit
    , ModuVerse(..)
    , getNames
    , getInfo
    -- , putName
    , putModule
    , putModuleAnew
    , findModule
    , delName
    , getExtensions
    , modifyExtensions
    -- , getSourceDirs
    -- , modifySourceDirs
    , parseModule
    , parseModuleMaybe
    , loadModule
    , unloadModule
    ) where

import Control.Applicative ((<$>))
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, MonadCatchIO, throw)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Map as Map (delete, empty, insert, keys, lookup, Map)
import Data.Maybe (fromMaybe)
import Data.Set as Set (fromList, Set)
import qualified Language.Haskell.Exts.Annotated as A (Module(..), ModuleHead(..), parseFileWithComments)
import Language.Haskell.Exts.Annotated.Simplify (sModuleName)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Extension (Extension)
import qualified Language.Haskell.Exts.Parser as Exts (defaultParseMode, fromParseResult, ParseMode(extensions, parseFilename), ParseResult)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax as S (ModuleName(..))
import Language.Haskell.Modules.SourceDirs (pathKey, pathKeyMaybe, PathKey(..), SourceDirs(..), modulePathBase)
import Language.Haskell.Modules.Util.QIO (MonadVerbosity, qLnPutStr, quietly)
import System.IO.Error (isDoesNotExistError, isUserError)

deriving instance Ord Comment

type ModuleInfo = (A.Module SrcSpanInfo, String, [Comment])
--type ModuleMap = Map S.ModuleName ModuleInfo

moduleName :: A.Module a -> S.ModuleName
moduleName (A.Module _ (Just (A.ModuleHead _ x _ _)) _ _ _) = sModuleName x
moduleName _ = S.ModuleName "Main"

data ModuVerseState =
    ModuVerseState { moduleNames_ :: Maybe (Map S.ModuleName ModuleInfo)
                   , moduleInfo_ :: Map PathKey ModuleInfo
                   , extensions_ :: [Extension]
                   , sourceDirs_ :: [FilePath]
                   -- ^ Top level directories to search for source files and
                   -- imports.  These directories would be the value used in the
                   -- hs-source-dirs parameter of a cabal file, and passed to ghc
                   -- via the -i option.
                   } deriving (Eq, Ord, Show)

moduVerseInit :: ModuVerseState
moduVerseInit =
    ModuVerseState { moduleNames_ = Nothing
                   , moduleInfo_ = Map.empty
                   , extensions_ = Exts.extensions Exts.defaultParseMode
                   , sourceDirs_ = ["."] }

getNames :: ModuVerse m => m (Set S.ModuleName)
getNames = getModuVerse >>= return . Set.fromList . keys . fromMaybe (error "No modules in ModuVerse, use putModule") . moduleNames_

getInfo :: ModuVerse m => S.ModuleName -> m (Maybe ModuleInfo)
getInfo name = getModuVerse >>= return . Map.lookup name . fromMaybe (error "No modules in ModuVerse, use putModule") . moduleNames_

putName :: ModuVerse m => S.ModuleName -> ModuleInfo -> m ()
putName name info = modifyModuVerse (\ s -> s {moduleNames_ = Just (Map.insert name info (fromMaybe Map.empty (moduleNames_ s)))})

putModule :: (ModuVerse m, MonadVerbosity m) => String -> m ()
putModule name = pathKey (modulePathBase "hs" (S.ModuleName name)) >>= parseModule >>= putName (S.ModuleName name)

putModuleAnew :: (ModuVerse m, MonadVerbosity m) => String -> m ()
putModuleAnew name = pathKey (modulePathBase "hs" (S.ModuleName name)) >>= loadModule >>= putName (S.ModuleName name)

findModule :: (ModuVerse m, MonadVerbosity m) => String -> m (Maybe ModuleInfo)
findModule name = pathKeyMaybe (modulePathBase "hs" (S.ModuleName name)) >>= parseModuleMaybe

delName :: ModuVerse m => S.ModuleName -> m ()
delName name = modifyModuVerse (\ s -> s { moduleNames_ = Just (Map.delete name (fromMaybe Map.empty (moduleNames_ s)))
                                         , moduleInfo_ = Map.empty })

class (MonadIO m, MonadCatchIO m, Functor m) => ModuVerse m where
    getModuVerse :: m ModuVerseState
    modifyModuVerse :: (ModuVerseState -> ModuVerseState) -> m ()

getExtensions :: ModuVerse m => m [Extension]
getExtensions = getModuVerse >>= return . extensions_

modifyExtensions :: ModuVerse m => ([Extension] -> [Extension]) -> m ()
modifyExtensions f = modifyModuVerse (\ s -> s {extensions_ = f (extensions_ s)})

instance ModuVerse m => SourceDirs m where
    putDirs xs = modifyModuVerse (\ s -> s {sourceDirs_ = xs})
    getDirs = getModuVerse >>= return . sourceDirs_

{-
getSourceDirs :: ModuVerse m => m [FilePath]
getSourceDirs = getModuVerse >>= return . sourceDirs_

-- | Modify the list of directories that will be searched for source
-- files, in a similar way to the Hs-Source-Dirs field in a cabal
-- file.  Default is @[\".\"]@.
modifySourceDirs :: ModuVerse m => ([FilePath] -> [FilePath]) -> m ()
modifySourceDirs f = modifyModuVerse (\ p -> p {sourceDirs_ = f (sourceDirs_ p)})
-}

parseModule :: (ModuVerse m, MonadVerbosity m) => PathKey -> m ModuleInfo
parseModule key = parseModuleMaybe (Just key) >>= maybe (error $ "parseModule - not found: " ++ show key) return

parseModuleMaybe :: (ModuVerse m, MonadVerbosity m) => Maybe PathKey -> m (Maybe ModuleInfo)
parseModuleMaybe Nothing = return Nothing
parseModuleMaybe (Just key) =
    (look >>= load) `IO.catch` (\ (e :: IOError) -> if isDoesNotExistError e || isUserError e then return Nothing else throw e)
    where
      look =
          do verse <- getModuVerse
             return $ Map.lookup key (moduleInfo_ verse)
      load (Just x) = return (Just x)
      load Nothing = Just <$> loadModule key

-- | Force a possibly cached module to be reloaded.
loadModule :: (ModuVerse m, MonadVerbosity m) => PathKey -> m ModuleInfo
loadModule key =
    do text <- liftIO $ readFile (unPathKey key)
       quietly $ qLnPutStr ("parsing " ++ unPathKey key)
       (parsed, comments) <- parseFileWithComments (unPathKey key) >>= return . Exts.fromParseResult
       modifyModuVerse (\ x -> x {moduleInfo_ = Map.insert key (parsed, text, comments) (moduleInfo_ x)})
       return (parsed, text, comments)

unloadModule :: (ModuVerse m, MonadVerbosity m) => FilePath -> m ()
unloadModule path =
    do key <- pathKey path
       modifyModuVerse (\ x -> x {moduleInfo_ = Map.delete key (moduleInfo_ x)})

-- | Run 'A.parseFileWithComments' with the extensions stored in the state.
parseFileWithComments :: ModuVerse m => FilePath -> m (Exts.ParseResult (A.Module SrcSpanInfo, [Comment]))
parseFileWithComments path =
    do exts <- getExtensions
       liftIO (A.parseFileWithComments (Exts.defaultParseMode {Exts.extensions = exts, Exts.parseFilename = path}) path)
