{-# LANGUAGE CPP, PackageImports, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.SourceDirs
    ( SourceDirs(..)
    , modifyDirs
    , PathKey(..)
    , pathKey
    , modulePath
    , modulePathBase
    , RelPath(..)
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
import System.IO.Error (isDoesNotExistError, isUserError)

class MonadCatchIO m => SourceDirs m where
    putDirs :: [FilePath] -> m ()
    getDirs :: m [FilePath]

modifyDirs :: SourceDirs m => ([FilePath] -> [FilePath]) -> m ()
modifyDirs f = getDirs >>= putDirs . f

-- | A FilePath that can be assumed to be unique.
newtype PathKey = PathKey {unPathKey :: FilePath} deriving (Eq, Ord, Show)

pathKey :: SourceDirs m => RelPath -> m PathKey
-- pathKey path = PathKey <$> liftIO (canonicalizePath path)
pathKey path = findSourcePath path >>= liftIO . canonicalizePath >>= return . PathKey

-- | Search the path directory list, preferring an already existing file, but
-- if there is none construct one using the first element of the directory list.
modulePath :: SourceDirs m => String -> S.ModuleName -> m FilePath
modulePath ext name =
    findSourcePath relPath `IO.catch` (\ (_ :: IOError) -> makePath)
    where
      makePath =
          do dirs <- getDirs
             case dirs of
               [] -> return path -- should this be an error?
               (d : _) -> return $ d </> path
      relPath@(RelPath path) = modulePathBase ext name

-- | Construct the base of a module path.
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

newtype RelPath = RelPath {unRelPath :: FilePath} deriving (Eq, Ord, Show)

-- | Search the path directory list for a source file that already exists.
-- FIXME: this should return a Maybe.
findSourcePath :: SourceDirs m => RelPath -> m FilePath
findSourcePath (RelPath path) =
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
