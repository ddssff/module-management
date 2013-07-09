{-# LANGUAGE CPP, PackageImports, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.ModuVerse
    ( ModuleInfo
    , moduleName
    , PathKey(..)
    , pathKey
    , ModuVerseState
    , moduVerseInit
    , ModuVerse(..)
    , getNames
    , getInfo
    , putName
    , delName
    , getExtensions
    , modifyExtensions
    , getSourceDirs
    , modifySourceDirs
    , parseModule
    , parseModule'
    , loadModule
    , unloadModule
    , modulePath
    , modulePathBase
    ) where

import Control.Applicative ((<$>))
import Control.Monad as List (mapM)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, MonadCatchIO, throw)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map as Map (Map, empty, fromList, insert, lookup, keys, delete)
import Data.Maybe (fromMaybe)
import Data.Set as Set (Set, fromList)
import qualified Language.Haskell.Exts.Annotated as A (Module(..), ModuleHead(..), parseFileWithComments)
import Language.Haskell.Exts.Annotated.Simplify (sModuleName)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Extension (Extension)
import qualified Language.Haskell.Exts.Parser as Exts (defaultParseMode, ParseMode(extensions, parseFilename), ParseResult, fromParseResult)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax as S (ModuleName(..))
import Language.Haskell.Modules.Util.QIO (MonadVerbosity, qLnPutStr, quietly)
import System.Directory (canonicalizePath, doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), (<.>))
import System.IO.Error (isDoesNotExistError)

deriving instance Ord Comment

type ModuleInfo = (A.Module SrcSpanInfo, String, [Comment])
--type ModuleMap = Map S.ModuleName ModuleInfo

moduleName :: A.Module a -> S.ModuleName
moduleName (A.Module _ (Just (A.ModuleHead _ x _ _)) _ _ _) = sModuleName x
moduleName _ = S.ModuleName "Main"

-- | A FilePath that can be assumed to be unique.
newtype PathKey = PathKey {unPathKey :: FilePath} deriving (Eq, Ord, Show)

pathKey :: (MonadIO m, Functor m) => FilePath -> m PathKey
pathKey path = PathKey <$> liftIO (canonicalizePath path)

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
getNames = getModuVerse >>= return . Set.fromList . keys . fromMaybe (error "No modules in ModuVerse, use putName") . moduleNames_

getInfo :: ModuVerse m => S.ModuleName -> m (Maybe ModuleInfo)
getInfo name = getModuVerse >>= return . Map.lookup name . fromMaybe (error "No modules in ModuVerse, use putName") . moduleNames_

putName :: ModuVerse m => S.ModuleName -> ModuleInfo -> m ()
putName name info = modifyModuVerse (\ s -> s {moduleNames_ = Just (Map.insert name info (fromMaybe Map.empty (moduleNames_ s)))})

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

getSourceDirs :: ModuVerse m => m [FilePath]
getSourceDirs = getModuVerse >>= return . sourceDirs_

-- | Modify the list of directories that will be searched for source
-- files, in a similar way to the Hs-Source-Dirs field in a cabal
-- file.  Default is @[\".\"]@.
modifySourceDirs :: ModuVerse m => ([FilePath] -> [FilePath]) -> m ()
modifySourceDirs f = modifyModuVerse (\ p -> p {sourceDirs_ = f (sourceDirs_ p)})

parseModule :: (ModuVerse m, MonadVerbosity m) => FilePath -> m ModuleInfo
parseModule path = parseModule' path >>= maybe (error $ "parseModule - not found: " ++ path) return

parseModule' :: (ModuVerse m, MonadVerbosity m) => FilePath -> m (Maybe ModuleInfo)
parseModule' path =
    (look >>= load) `IO.catch` (\ (e :: IOError) -> if isDoesNotExistError e then return Nothing else throw e)
    where
      look =
          do key <- pathKey path
             verse <- getModuVerse
             return $ Map.lookup key (moduleInfo_ verse)
      load (Just x) = return (Just x)
      load Nothing = Just <$> loadModule path

-- | Force a possibly cached module to be reloaded.
loadModule :: (ModuVerse m, MonadVerbosity m) => FilePath -> m ModuleInfo
loadModule path =
    do key <- pathKey path
       text <- liftIO $ readFile path
       qLnPutStr ("parsing " ++ unPathKey key ++ ", size=" ++ show (length text))
       (parsed, comments) <- parseFileWithComments path >>= return . Exts.fromParseResult
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

#if 0
    do verse <- getModuVerse
       key <- pathKey path
       case Map.lookoup key (moduleInfo_ verse) of
         Just x -> return x
         Nothing ->
             do text <- liftIO $ readFile path
                (parsed, comments) <- parseFileWithComments path >>= return . Exts.fromParseResult
                modifyModuVerse (\ verse -> verse { moduleInfo_ = Map.insert key (parsed, text, comments) (moduleInfo_ verse)
                                                  , moduleNameMap_ = Map.insertWith union (moduleName parsed) (singleton key) (moduleNameMap_ verse) })
                return (parsed, text, comments)
#endif

-- | Search the path directory list, preferring an already existing file, but
-- if there is none construct one using the first element of the directory list.
modulePath :: ModuVerse m => S.ModuleName -> m FilePath
modulePath name =
    findSourcePath (modulePathBase name) `IO.catch` (\ (_ :: IOError) -> makePath)
    where
      makePath =
          do dirs <- sourceDirs_ <$> getModuVerse
             case dirs of
               [] -> return (modulePathBase name) -- should this be an error?
               (d : _) -> return $ d </> modulePathBase name

-- | Construct the base of a module path.
modulePathBase :: S.ModuleName -> FilePath
modulePathBase (S.ModuleName name) =
    map f name <.> "hs"
    where
      f '.' = '/'
      f c = c

-- | Search the path directory list for a source file that already exists.
findSourcePath :: ModuVerse m => FilePath -> m FilePath
findSourcePath path =
    findFile =<< (sourceDirs_ <$> getModuVerse)
    where
      findFile (dir : dirs) =
          do let x = dir </> path
             exists <- liftIO $ doesFileExist x
             if exists then return x else findFile dirs
      findFile [] =
          do -- Just building an error message here
             here <- liftIO getCurrentDirectory
             dirs <- sourceDirs_ <$> getModuVerse
             liftIO . throw . userError $ "findSourcePath failed, cwd=" ++ here ++ ", dirs=" ++ show dirs ++ ", path=" ++ path
