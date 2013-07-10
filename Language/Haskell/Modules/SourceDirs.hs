{-# LANGUAGE CPP, PackageImports, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.SourceDirs
    ( SourceDirs(..)
    , modifyDirs
    , PathKey(..)
    , pathKey
    , modulePath
    , modulePathBase
    , findSourcePath
    ) where

import Control.Applicative ((<$>))
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, MonadCatchIO, throw)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map as Map (Map, empty, insert, lookup, keys, delete)
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

class MonadCatchIO m => SourceDirs m where
    putDirs :: [FilePath] -> m ()
    getDirs :: m [FilePath]

modifyDirs :: SourceDirs m => ([FilePath] -> [FilePath]) -> m ()
modifyDirs f = getDirs >>= putDirs . f

-- | A FilePath that can be assumed to be unique.
newtype PathKey = PathKey {unPathKey :: FilePath} deriving (Eq, Ord, Show)

pathKey :: SourceDirs m => FilePath -> m PathKey
-- pathKey path = PathKey <$> liftIO (canonicalizePath path)
pathKey path = findSourcePath path >>= liftIO . canonicalizePath >>= return . PathKey

-- | Search the path directory list, preferring an already existing file, but
-- if there is none construct one using the first element of the directory list.
modulePath :: SourceDirs m => S.ModuleName -> m FilePath
modulePath name =
    findSourcePath (modulePathBase name) `IO.catch` (\ (_ :: IOError) -> makePath)
    where
      makePath =
          do dirs <- getDirs
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
findSourcePath :: SourceDirs m => FilePath -> m FilePath
findSourcePath path =
    findFile =<< getDirs
    where
      findFile (dir : dirs) =
          do let x = dir </> path
             exists <- liftIO $ doesFileExist x
             if exists then return x else findFile dirs
      findFile [] =
          do -- Just building an error message here
             here <- liftIO getCurrentDirectory
             dirs <- getDirs
             liftIO . throw . userError $ "findSourcePath failed, cwd=" ++ here ++ ", dirs=" ++ show dirs ++ ", path=" ++ path
