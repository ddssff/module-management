{-# LANGUAGE CPP, PackageImports, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.SourceDirs
    ( SourceDirs(..)
    , modifyDirs
    , PathKey(..)
    , pathKey
    , pathKeyMaybe
    , modulePath
    , modulePathBase
    ) where

import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, MonadCatchIO, throw)
import Control.Monad.Trans (liftIO, MonadIO)
import Language.Haskell.Exts.Syntax as S (ModuleName(..))
import System.Directory (canonicalizePath, doesFileExist, getCurrentDirectory)
import System.FilePath ((<.>), (</>))

class MonadCatchIO m => SourceDirs m where
    putDirs :: [FilePath] -> m ()
    getDirs :: m [FilePath]

modifyDirs :: SourceDirs m => ([FilePath] -> [FilePath]) -> m ()
modifyDirs f = getDirs >>= putDirs . f

-- | A FilePath that can be assumed to be unique.
newtype PathKey = PathKey {unPathKey :: FilePath} deriving (Eq, Ord, Show)

pathKey :: SourceDirs m => FilePath -> m PathKey
-- pathKey path = PathKey <$> liftIO (canonicalizePath path)
pathKey path = findFile path >>= liftIO . canonicalizePath >>= return . PathKey

pathKeyMaybe :: SourceDirs m => FilePath -> m (Maybe PathKey)
pathKeyMaybe path = findFileMaybe path >>= maybe (return Nothing) (\ path' -> liftIO (canonicalizePath path') >>= return . Just . PathKey)

-- | Search the path directory list, preferring an already existing file, but
-- if there is none construct one using the first element of the directory list.
modulePath :: SourceDirs m => String -> S.ModuleName -> m FilePath
modulePath ext name =
    findFile path `IO.catch` (\ (_ :: IOError) -> makePath)
    where
      makePath =
          do dirs <- getDirs
             case dirs of
               [] -> return path -- should this be an error?
               (d : _) -> return $ d </> path
      path = modulePathBase ext name

-- | Construct the base of a module path.
modulePathBase :: String -> S.ModuleName -> FilePath
modulePathBase ext (S.ModuleName name) =
    base <.> ext
    where base = case ext of
                   "hs" -> map f name
                   "lhs" -> map f name
                   "imports" -> name
                   _ -> error $ "Unsupported extension: " ++ show ext
          f '.' = '/'
          f c = c

-- | Search the path directory list for a source file that already exists.
-- FIXME: this should return a Maybe.
findFile :: SourceDirs m => FilePath -> m FilePath
findFile path =
    findFileMaybe path >>=
    maybe (do here <- liftIO getCurrentDirectory
              dirs <- getDirs
              liftIO . throw . userError $ "findFile failed, cwd=" ++ here ++ ", dirs=" ++ show dirs ++ ", path=" ++ path)
          return

findFileMaybe :: SourceDirs m => FilePath -> m (Maybe FilePath)
findFileMaybe path =
    getDirs >>= f
    where
      f (dir : dirs) =
          do let x = dir </> path
             exists <- liftIO $ doesFileExist x
             if exists then return (Just x) else f dirs
      f [] = return Nothing
