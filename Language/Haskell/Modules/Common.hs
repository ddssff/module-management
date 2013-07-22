{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Common
    ( groupBy'
    , withCurrentDirectory
    , ModuleResult(Created, Modified, Removed, Unchanged)
    , doResult
    , fixExport
    ) where

import Control.Exception.Lifted as IO (bracket, catch, throw)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.List (groupBy, sortBy)
import Data.Monoid ((<>))
import Data.Sequence as Seq (Seq, (|>))
import qualified Language.Haskell.Exts.Annotated as A (ExportSpec)
import Language.Haskell.Exts.Annotated.Simplify (sExportSpec)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(EModuleContents), ModuleName(..))
import Language.Haskell.Modules.ModuVerse (delName, ModuVerse, putModuleAnew, unloadModule)
import Language.Haskell.Modules.SourceDirs (modulePath, PathKey(..))
import Language.Haskell.Modules.Util.DryIO (createDirectoryIfMissing, MonadDryRun(..), removeFileIfPresent, replaceFile, tildeBackup)
import Language.Haskell.Modules.Util.QIO (MonadVerbosity(..), qLnPutStr, quietly)
import Prelude hiding (writeFile, writeFile)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath (dropExtension, takeDirectory)
import System.IO.Error (isDoesNotExistError)

-- | Convert a compare function into an (==)
toEq :: Ord a => (a -> a -> Ordering) -> (a -> a -> Bool)
toEq cmp a b =
    case cmp a b of
      EQ -> True
      _ -> False

-- | Combine sortBy and groupBy
groupBy' :: Ord a => (a -> a -> Ordering) -> [a] -> [[a]]
groupBy' cmp xs = groupBy (toEq cmp) $ sortBy cmp xs

withCurrentDirectory :: (MonadIO m, MonadBaseControl IO m) => FilePath -> m a -> m a
withCurrentDirectory path action =
    bracket (liftIO getCurrentDirectory >>= \ save -> liftIO (setCurrentDirectory path) >> return save)
            (liftIO . setCurrentDirectory)
            (const action)

data ModuleResult
    = Unchanged S.ModuleName PathKey
    | Removed S.ModuleName PathKey
    | Modified S.ModuleName PathKey String
    | Created S.ModuleName String
    deriving (Show, Eq, Ord)

-- | It is tempting to put import cleaning into these operations, but
-- that needs to be done after all of these operations are completed
-- so that all the compiles required for import cleaning succeed.  On
-- the other hand, we might be able to maintain the moduVerse here.
doResult :: (ModuVerse m, MonadDryRun m, MonadVerbosity m) => ModuleResult -> m ModuleResult
doResult x@(Unchanged name _) =
    do quietly (qLnPutStr ("unchanged: " ++ prettyPrint name))
       return x
doResult x@(Removed name key) =
    do qLnPutStr ("removed: " ++ prettyPrint name)
       let path = unPathKey key
       unloadModule key
       -- I think this event handler is redundant.
       removeFileIfPresent path `IO.catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
       delName name
       return x

doResult x@(Modified name _ text) =
    do qLnPutStr ("modified: " ++ prettyPrint name)
       path <- modulePath "hs" name
       -- qLnPutStr ("modifying " ++ show path)
       -- (quietly . quietly . quietly . qPutStr $ " new text: " ++ show text)
       replaceFile tildeBackup path text
       putModuleAnew name
       return x

doResult x@(Created name text) =
    do qLnPutStr ("created: " ++ prettyPrint name)
       path <- modulePath "hs" name
       -- qLnPutStr ("creating " ++ show path)
       -- (quietly . quietly . quietly . qPutStr $ " containing " ++ show text)
       createDirectoryIfMissing True (takeDirectory . dropExtension $ path)
       replaceFile tildeBackup path text
       putModuleAnew name
       return x

-- | Update an export spec.  The only thing we might need to change is
-- re-exports, of the form "module Foo".
fixExport :: [S.ModuleName] -> S.ModuleName -> S.ModuleName
          -> A.ExportSpec l -> String -> String -> String -> Seq String -> Seq String
fixExport inNames outName thisName e pref s suff r =
    case sExportSpec e of
      S.EModuleContents name
          -- when building the output module, omit re-exports of input modules
          | thisName == outName && elem name inNames -> r
          -- when building other modules, update re-exports of input
          -- modules to be a re-export of the output module.
          | elem name inNames -> r |> pref <> prettyPrint (S.EModuleContents outName) <> suff
          -- Anything else is unchanged
      _ -> r |> pref <> s <> suff
