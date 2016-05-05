{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Common
    ( groupBy'
    , withCurrentDirectory
    , ModuleResult(..)
    , doResult
    , reportResult
    , fixExport
    ) where

import Control.Exception.Lifted as IO (bracket, catch, throw)
import Control.Monad (when)
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
import Language.Haskell.Modules.SourceDirs (modulePath, PathKey(..), APath(..))
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

-- | Perform an action with the working directory set to @path@.
withCurrentDirectory :: (MonadIO m, MonadBaseControl IO m) => FilePath -> m a -> m a
withCurrentDirectory path action =
    bracket (liftIO getCurrentDirectory >>= \ save -> liftIO (setCurrentDirectory path) >> return save)
            (liftIO . setCurrentDirectory)
            (const action)

data ModuleResult
    = Unchanged S.ModuleName PathKey
    | ToBeRemoved S.ModuleName PathKey
    | JustRemoved S.ModuleName PathKey
    | ToBeModified S.ModuleName PathKey String
    | JustModified S.ModuleName PathKey
    | ToBeCreated S.ModuleName String
    | JustCreated S.ModuleName PathKey
    deriving (Show, Eq, Ord)

reportResult :: ModuleResult -> String
reportResult (Unchanged _ key) = "unchanged " ++ show key
reportResult (JustModified _ key) = "modified " ++ show key
reportResult (JustCreated _ key) = "created " ++ show key
reportResult (JustRemoved _ key) = "removed " ++ show key
reportResult (ToBeModified _ key _) = "to be modified " ++ show key
reportResult (ToBeCreated name _) = "to be created " ++ show name
reportResult (ToBeRemoved _ key) = "to be removed: " ++ show key

-- | It is tempting to put import cleaning into these operations, but
-- that needs to be done after all of these operations are completed
-- so that all the compiles required for import cleaning succeed.  On
-- the other hand, we might be able to maintain the moduVerse here.
doResult :: (ModuVerse m, MonadDryRun m, MonadVerbosity m) => ModuleResult -> m ModuleResult
doResult x@(Unchanged name _) =
    do quietly (qLnPutStr ("unchanged: " ++ prettyPrint name))
       return x
doResult (ToBeRemoved name key) =
    do qLnPutStr ("removing: " ++ prettyPrint name)
       let path = unPathKey key
       unloadModule key
       -- I think this event handler is redundant.
       removeFileIfPresent path `IO.catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
       delName name
       return $ JustRemoved name key

doResult (ToBeModified name key text) =
    do qLnPutStr ("modifying: " ++ prettyPrint name)
       let path = unPathKey key
       -- qLnPutStr ("modifying " ++ show path)
       -- (quietly . quietly . quietly . qPutStr $ " new text: " ++ show text)
       replaceFile tildeBackup path text
       _key <- putModuleAnew name
       return $ JustModified name key

doResult (ToBeCreated name text) =
    do qLnPutStr ("creating: " ++ prettyPrint name)
       (path, name') <- modulePath "hs" name
       when (name /= name') (qLnPutStr ("Module name mismatch: " ++ show name ++ " vs. " ++ show name'))
       -- qLnPutStr ("creating " ++ show path)
       -- (quietly . quietly . quietly . qPutStr $ " containing " ++ show text)
       createDirectoryIfMissing True (takeDirectory . dropExtension . unAPath $ path)
       replaceFile tildeBackup (unAPath path) text
       key <- putModuleAnew name
       return $ JustCreated name key
doResult x@(JustCreated {}) = return x
doResult x@(JustModified {}) = return x
doResult x@(JustRemoved {}) = return x

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
