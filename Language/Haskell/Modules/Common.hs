{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, FlexibleInstances, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Common
    ( groupBy'
    , withCurrentDirectory
    , ModuleResult(..)
    , doResult
    , reportResult
    , fixExport
    ) where

import Debug.Trace
import Control.Exception.Lifted as IO (bracket, catch, throw)
import Control.Lens ((%=))
--import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.List (groupBy, sortBy)
import Data.Map as Map (delete)
import Data.Monoid ((<>))
import Data.Sequence as Seq (Seq, (|>))
#if MIN_VERSION_haskell_src_exts(1,18,0)
import qualified Language.Haskell.Exts as A (ExportSpec)
#else
import qualified Language.Haskell.Exts.Annotated as A (ExportSpec)
import Language.Haskell.Exts.Annotated.Simplify (sExportSpec)
#endif
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(EModuleContents))
import Language.Haskell.Modules.ModuVerse (ModuVerse, putModuleAnew, unloadModule, moduVerse {-modulesOrig, moduleKey, modulesNew-})
import Language.Haskell.Modules.SourceDirs (ModKey(..), Path(findFileMaybe), AHsDir(..))
import Language.Haskell.Modules.Util.DryIO (createDirectoryIfMissing, MonadDryRun(..), removeFileIfPresent, replaceFile, tildeBackup)
import Language.Haskell.Modules.Util.QIO (MonadVerbosity(..) {-, qLnPutStr, quietly-})
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
    = Unchanged ModKey
    | ToBeRemoved ModKey
    | JustRemoved ModKey
    | ToBeModified ModKey String
    | JustModified ModKey
    | ToBeCreated ModKey String
    | JustCreated ModKey
    deriving (Show, Eq, Ord)

reportResult :: ModuleResult -> String
reportResult (Unchanged key) = "unchanged " ++ show key
reportResult (JustModified key) = "modified " ++ show key
reportResult (JustCreated key) = "created " ++ show key
reportResult (JustRemoved key) = "removed " ++ show key
reportResult (ToBeModified key _) = "to be modified " ++ show key
reportResult (ToBeCreated key _) = "to be created " ++ show key
reportResult (ToBeRemoved key) = "to be removed: " ++ show key

-- | It is tempting to put import cleaning into these operations, but
-- that needs to be done after all of these operations are completed
-- so that all the compiles required for import cleaning succeed.  On
-- the other hand, we might be able to maintain the moduVerse here.
doResult :: (ModuVerse m, MonadDryRun m, MonadVerbosity m) => ModuleResult -> m ModuleResult
doResult x@(Unchanged _) =
    do -- quietly (qLnPutStr ("unchanged: " ++ prettyPrint name))
       return x
doResult (ToBeRemoved key) =
    do -- qLnPutStr ("removing: " ++ prettyPrint name)
       let path = _modKey key
       unloadModule key
       -- I think this event handler is redundant.
       removeFileIfPresent path `IO.catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
       moduVerse %= Map.delete key
       return $ JustRemoved key

doResult (ToBeModified key text) =
    do -- qLnPutStr ("modifying: " ++ prettyPrint name)
       let path = _modKey key
       -- qLnPutStr ("modifying " ++ show path)
       -- (quietly . quietly . quietly . qPutStr $ " new text: " ++ show text)
       replaceFile tildeBackup path text
       _key <- putModuleAnew key
       return $ JustModified key

doResult (ToBeCreated key text) =
    do trace ("creating: " ++ show key) (return ())
       Just (path, _name) <- findFileMaybe key
       -- when (name /= name') (qLnPutStr ("Module name mismatch: " ++ show name ++ " vs. " ++ show name'))
       -- qLnPutStr ("creating " ++ show path)
       -- (quietly . quietly . quietly . qPutStr $ " containing " ++ show text)
       createDirectoryIfMissing True (takeDirectory . dropExtension . unAHsDir $ path)
       replaceFile tildeBackup (unAHsDir path) text
       putModuleAnew key
       return $ JustCreated key
doResult x@(JustCreated {}) = return x
doResult x@(JustModified {}) = return x
doResult x@(JustRemoved {}) = return x

-- | Update an export spec.  The only thing we might need to change is
-- re-exports, of the form "module Foo".
fixExport :: [ModKey] -> ModKey -> ModKey
          -> A.ExportSpec l -> String -> String -> String -> Seq String -> Seq String
fixExport inKeys outKey thisKey e pref s suff r =
    case fmap (const ()) e of
      S.EModuleContents () name
          -- when building the output module, omit re-exports of input modules
          | thisKey == outKey && elem name (map _modName inKeys) -> r
          -- when building other modules, update re-exports of input
          -- modules to be a re-export of the output module.
          | elem name (map _modName inKeys) -> r |> pref <> prettyPrint (S.EModuleContents () (_modName outKey)) <> suff
          -- Anything else is unchanged
      _ -> r |> pref <> s <> suff
