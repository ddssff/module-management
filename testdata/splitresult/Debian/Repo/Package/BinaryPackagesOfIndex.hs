{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.BinaryPackagesOfIndex
    ( binaryPackagesOfIndex
    ) where

import Control.Exception as E (SomeException)
import "mtl" Control.Monad.Trans (MonadIO(..))
import Debian.Arch (Arch(Source))
import Debian.Repo.Package.GetPackages (getPackages)
import Debian.Repo.Types.PackageIndex (BinaryPackage, PackageIndex(packageIndexArch))
import Debian.Repo.Types.Release (Release)
import Debian.Repo.Types.Repo (RepoKey)
import Debian.Repo.Types.Repository (MonadRepoCache)
      --toLazy s = L.fromChunks [s]
      --showStream :: Either Exception L.ByteString -> IO (Either Exception L.ByteString)
      --showStream x@(Left e) = hPutStrLn stderr (show uri' ++ " - exception: " ++ show e) >> return x
      --showStream x@(Right s) = hPutStrLn stderr (show uri' ++ " - stream length: " ++ show (L.length s)) >> return x

-- | Get the contents of a package index
binaryPackagesOfIndex :: MonadRepoCache m => RepoKey -> Release -> PackageIndex -> m (Either SomeException [BinaryPackage])
binaryPackagesOfIndex repo release index =
    case packageIndexArch index of
      Source -> return (Right [])
      _ -> liftIO $ getPackages repo release index -- >>= return . either Left (Right . List.map (toBinaryPackage index . packageInfo))
