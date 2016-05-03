{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, PackageImports, ScopedTypeVariables, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.SourceDirs
    ( SourceDirs(..)
    , modifyHsSourceDirs
    , withModifiedDirs
    , withDirs
    , RelPath(..)
    , PathKey(..)
    , APath(..)
    , pathKey
    , Path(..)
    , modulePath
    , modulePathBase
    ) where

import Control.Exception.Lifted as IO (catch, throw, bracket)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Language.Haskell.Exts.Syntax as S (ModuleName(..))
import System.Directory (canonicalizePath, doesFileExist, getCurrentDirectory)
import System.FilePath ((<.>), (</>))

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

-- | A FilePath that can be assumed to be unique.
newtype PathKey = PathKey {unPathKey :: FilePath} deriving (Eq, Ord, Show)

-- | A FilePath that is relative to the SourceDir list
newtype RelPath = RelPath {unRelPath :: FilePath} deriving (Eq, Ord, Show)

-- | A regular filepath with a wrapper
newtype APath = APath {unAPath :: FilePath} deriving (Eq, Ord, Show)

-- | Search the path directory list, preferring an already existing file, but
-- if there is none construct one using the first element of the directory list.
modulePath :: SourceDirs m => String -> S.ModuleName -> m APath
modulePath ext name =
    findFile path `IO.catch` (\ (_ :: IOError) -> makePath)
    where
      makePath =
          do dirs <- getHsSourceDirs
             case dirs of
               [] -> error "Empty $PATH" -- return (APath (unRelPath path)) -- should this be an error?
               (d : _) -> return . APath $ d </> unRelPath path
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

class Path a where
    findFileMaybe :: SourceDirs m => a -> m (Maybe APath)
    pathKeyMaybe :: SourceDirs m => a -> m (Maybe PathKey)

instance Path RelPath where
    findFileMaybe (RelPath path) =
        getHsSourceDirs >>= f
        where
          f (dir : dirs) =
              do let x = dir </> path
                 exists <- liftIO $ doesFileExist x
                 if exists then return (Just (APath x)) else f dirs
          f [] = return Nothing
    pathKeyMaybe path =
        findFileMaybe path >>= maybe (return Nothing) (\ (APath path') -> liftIO (canonicalizePath path') >>= return . Just . PathKey)

instance Path PathKey where
    findFileMaybe (PathKey x) = return (Just (APath x))
    pathKeyMaybe x = return (Just x)

instance Path APath where
    findFileMaybe (APath x) =
        do exists <- liftIO $ doesFileExist x
           return $ if exists then Just (APath x) else Nothing
    pathKeyMaybe x =
        do mpath <- findFileMaybe x
           maybe (return Nothing) (\ (APath y) -> liftIO (canonicalizePath y) >>= return . Just . PathKey) mpath

-- | Find a source file using $PWD and the Hs-Source-Dirs directory list.
findFile :: (SourceDirs m, Path p, Show p) => p -> m APath
findFile path =
    findFileMaybe path >>=
    maybe (do here <- liftIO getCurrentDirectory
              dirs <- getHsSourceDirs
              liftIO . throw . userError $ "findFile failed, cwd=" ++ here ++ ", dirs=" ++ show dirs ++ ", path=" ++ show path)
          return

-- | 
pathKey :: (SourceDirs m, Path p, Show p) => p -> m PathKey
pathKey path = findFile path >>= liftIO . canonicalizePath . unAPath >>= return . PathKey
