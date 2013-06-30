{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.BinaryPackagesOfCachedIndex
    ( binaryPackagesOfCachedIndex
    ) where

import "mtl" Control.Monad.Trans (MonadIO(..))
import Data.List as List (map)
import Debian.Repo.Monads.Apt (insertBinaryPackages, lookupBinaryPackages, MonadApt(..), readParagraphs)
import Debian.Repo.Package.Internal.IndexCacheFile (indexCacheFile)
import Debian.Repo.Package.ToBinaryPackage (toBinaryPackage)
import Debian.Repo.Types.AptCache (AptCache(rootDir))
import Debian.Repo.Types.EnvPath (EnvRoot(rootPath))
import Debian.Repo.Types.PackageIndex (BinaryPackage, PackageIndex)
import Debian.Repo.Types.Release (Release)
import Debian.Repo.Types.Repo (RepoKey)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix (getFileStatus)

-- FIXME: assuming the index is part of the cache
binaryPackagesOfCachedIndex :: (MonadApt m, AptCache a) => a -> RepoKey -> Release -> PackageIndex -> m [BinaryPackage]
binaryPackagesOfCachedIndex cache repo release index =
    do state <- getApt
       let cached = lookupBinaryPackages path state
       status <- liftIO $ getFileStatus path
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = List.map (toBinaryPackage release index) paragraphs
                 putApt (insertBinaryPackages path (status, packages) state)
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache repo release index
