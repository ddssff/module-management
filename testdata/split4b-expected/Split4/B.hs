{-# LANGUAGE ScopedTypeVariables #-}
module Split4.B 
    ( sourcePackagesOfIndex
    , binaryPackagesOfIndex
    ) where
import Split4.A (getPackages)
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

-- | Get the contents of a package index
sourcePackagesOfIndex :: MonadRepoCache m => RepoKey -> Release -> PackageIndex -> m (Either SomeException [SourcePackage])
sourcePackagesOfIndex repo release index =
    case packageIndexArch index of
      Source -> liftIO (getPackages repo release index) >>= return . either Left (Right . List.map (toSourcePackage index . packageInfo))
      _ -> return (Right [])
