{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.SourcePackagesOfCachedIndex
    ( -- * Source and binary packages
      sourcePackagesOfCachedIndex
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import Control.Exception as E (catch)
import "mtl" Control.Monad.Trans (MonadIO(..))
import Data.List as List (map)
import Debian.Repo.Monads.Apt (insertSourcePackages, lookupSourcePackages, MonadApt(..), readParagraphs)
import Debian.Repo.Package.IndexCacheFile (indexCacheFile)
import Debian.Repo.Package.ToSourcePackage (toSourcePackage)
import Debian.Repo.Types.AptCache (AptCache(rootDir))
import Debian.Repo.Types.EnvPath (EnvRoot(rootPath))
import Debian.Repo.Types.PackageIndex (PackageIndex, SourcePackage)
import Debian.Repo.Types.Release (Release)
import Debian.Repo.Types.Repo (RepoKey)
import System.FilePath (takeDirectory)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix (getFileStatus)

sourcePackagesOfCachedIndex :: (AptCache a, MonadApt m) => a -> RepoKey -> Release -> PackageIndex -> m [SourcePackage]
sourcePackagesOfCachedIndex cache repo release index =
    do state <- getApt
       let cached = lookupSourcePackages path state
       status <- liftIO $ getFileStatus path `E.catch` (\ (_ :: IOError) -> error $ "Sources.list seems out of sync.  If a new release has been created you probably need to remove " ++ takeDirectory (rootPath (rootDir cache)) ++ " and try again - sorry about that.")
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = List.map (toSourcePackage index) paragraphs
                 putApt (insertSourcePackages path (status, packages) state)
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache repo release index


