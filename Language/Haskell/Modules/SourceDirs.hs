-- | Code for dealing with the ways ghc finds files based on module
-- name and its -i argument (aka the cabal hs-sources-list.)

{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, PackageImports, ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.SourceDirs
    ( SourceDirs(..)
    , modifyHsSourceDirs
    , withModifiedDirs
    , withDirs
    , RelPath(..)
    , ModKey(..)
    , AHsDir(..)
    , modulePath
    , modulePathBase
    , Path(..)
    , modKey
    ) where

import Control.Exception.Lifted as IO (catch, throw, bracket)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.List (intercalate, stripPrefix, uncons)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax as S (ModuleName(..))
import System.Directory (canonicalizePath, doesFileExist, getCurrentDirectory)
import System.FilePath ((<.>), (</>), dropExtension, joinPath, splitDirectories)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

class (MonadIO m, MonadBaseControl IO m) => SourceDirs m where
    putHsSourceDirs :: [FilePath] -> m ()
    -- ^ Set the list of directories that will be searched for
    -- imported modules.  Similar to the Hs-Source-Dirs field in the
    -- cabal file.
    getHsSourceDirs :: m [FilePath]

-- | Modify the list of directories that will be searched for imported
-- modules.
modifyHsSourceDirs :: SourceDirs m => ([FilePath] -> [FilePath]) -> m ()
modifyHsSourceDirs f = getHsSourceDirs >>= putHsSourceDirs . f

withDirs :: SourceDirs m => [FilePath] -> m a -> m a
withDirs dirs action = withModifiedDirs (const dirs) action

withModifiedDirs :: SourceDirs m => ([FilePath] -> [FilePath]) -> m a -> m a
withModifiedDirs f action = bracket (getHsSourceDirs >>= \save -> putHsSourceDirs (f save) >> return save) putHsSourceDirs (const action)

-- | A FilePath that can be assumed to be unique.  It is possible for
-- more than one module of the same name to be in the moduverse, in
-- particular there may be several Main modules that we want to work
-- on simultaneously.
data ModKey =
    ModKey
    { unModKey :: FilePath
      -- ^ The directory to which this module's path is relative.
      -- I.e., the element of Hs-Source-Dirs that makes this module
      -- visible.
    , unModName :: S.ModuleName -- ^ The module's name
    } deriving (Eq, Ord, Show)

instance Pretty ModKey where
    pPrint x = text (prettyPrint (unModName x) ++ case unModKey x of
                                                    "." -> ""
                                                    "" -> ""
                                                    _ -> "(in " ++ unModKey x ++ ")")

-- | A FilePath that is relative to the SourceDir list.
newtype RelPath = RelPath {unRelPath :: FilePath} deriving (Eq, Ord, Show)

-- | A regular filepath with a wrapper, used to represent a element of
-- the hs-source-dirs directory list.
newtype AHsDir = AHsDir {unAHsDir :: FilePath} deriving (Eq, Ord, Show)

-- | Search the path directory list, preferring an already existing file, but
-- if there is none construct one using the first element of the directory list.
modulePath :: SourceDirs m => String -> S.ModuleName -> m (AHsDir, S.ModuleName)
modulePath ext name =
    findFile path `IO.catch` (\ (_ :: IOError) -> makePath)
    where
      makePath =
          do dirs <- getHsSourceDirs
             case dirs of
               [] -> error "Empty $PATH" -- return (AHsDir (unRelPath path)) -- should this be an error?
               (d : _) -> return (AHsDir (d </> unRelPath path), pathToModule (unRelPath path))
      path = modulePathBase ext name

-- | Derive a relative FilePath from a module name based on the file
-- type inferred by the extension.  Thus, @modulePathBase "hs"
-- (ModuleName "System.Control.Monad")@ returns
-- @"System/Control/Monad.hs"@, while @modulePathBase "imports"
-- (ModuleName "System.Control.Monad")@ returns
-- @"System.Control.Monad.imports"@.
modulePathBase :: String -> S.ModuleName -> RelPath
modulePathBase ext (S.ModuleName name) =
    RelPath (base <.> ext)
    where base = case ext of
                   "hs" -> map f name
                   "lhs" -> map f name
                   "imports" -> name
                   _ -> error $ "Unsupported extension: " ++ show ext
          f '.' = '/'
          f c = c

-- | Something we can use to find a file containing a module.
class Path a where
    findFileMaybe :: SourceDirs m => a -> m (Maybe (AHsDir, S.ModuleName))
    modKeyMaybe :: SourceDirs m => a -> m (Maybe ModKey)

instance Path RelPath where
    findFileMaybe (RelPath path) =
        getHsSourceDirs >>= f
        where
          f (dir : dirs) =
              do let x = dir </> path
                 exists <- liftIO $ doesFileExist x
                 -- Compute the module name based on the path.  It
                 -- might be different in the module header, I believe
                 -- this is an error.
                 if exists then return (Just (AHsDir x, pathToModule path)) else f dirs
          f [] = return Nothing
    modKeyMaybe path =
        findFileMaybe path >>= maybe (return Nothing) (\ (AHsDir path', modName) -> liftIO (canonicalizePath path') >>= \path'' -> return (Just (ModKey path'' modName)))

instance Path ModKey where
    findFileMaybe (ModKey x modName) = return (Just (AHsDir x, modName))
    modKeyMaybe x = return (Just x)

instance Path AHsDir where
    findFileMaybe (AHsDir x) = do
      exists <- liftIO (doesFileExist x)
      -- This is a poor substitute for using canonicalizePath
      let x' = filter (/= ".") (splitDirectories x)
      if exists then getHsSourceDirs >>= f x' else return Nothing
        where
          f :: SourceDirs m => [FilePath] -> [FilePath] -> m (Maybe (AHsDir, S.ModuleName))
          f x' (dir : dirs) = do
            let dir' = filter (/= ".") (splitDirectories dir)
            case stripPrefix dir' x' of
              Just x'' -> return $ Just (AHsDir x, pathToModule (joinPath x''))
              Nothing -> f x' dirs
          f _ [] = return Nothing
    modKeyMaybe x =
        do mpath <- findFileMaybe x
           maybe (return Nothing) (\(AHsDir y, modName) -> liftIO (canonicalizePath y) >>= \path' -> return (Just (ModKey path' modName))) mpath

-- | Find a source file using $PWD and the Hs-Source-Dirs directory list.
findFile :: (SourceDirs m, Path p, Show p) => p -> m (AHsDir, S.ModuleName)
findFile path =
    findFileMaybe path >>=
    maybe (do here <- liftIO getCurrentDirectory
              dirs <- getHsSourceDirs
              liftIO . throw . userError $ "findFile failed, cwd=" ++ here ++ ", dirs=" ++ show dirs ++ ", path=" ++ show path)
          return

modKey :: (SourceDirs m, Path p, Show p) => p -> m ModKey
modKey path = findFile path >>= \(aPath, modName) -> liftIO (canonicalizePath (unAHsDir aPath)) >>= \path' -> return (ModKey path' modName)

-- Convert a path assumed to be relative to an element of
-- hs-source-dirs into a module name.
pathToModule :: FilePath -> S.ModuleName
pathToModule =
    (\(dirs, name) -> S.ModuleName (intercalate "." (dirs ++ [dropExtension name]))) . fromJust . unsnoc . filter (/= ".") . splitDirectories

-- swapped uncons
unsnoc :: [a] -> Maybe ([a], a)
unsnoc =
    fmap rev . fmap swap . uncons . reverse
    where
      rev :: ([a], a) -> ([a], a)
      rev (dirs, name) = (reverse dirs, name)
