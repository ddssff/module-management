{-# LANGUAGE ScopedTypeVariables #-}
module Split4.SourcePackagesOfIndex 
    ( sourcePackagesOfIndex
    ) where
import Split4.GetPackages (getPackages)
-- >>= return . either Left (Right . List.map (toBinaryPackage index . packageInfo))

-- | Get the contents of a package index
sourcePackagesOfIndex :: MonadRepoCache m => RepoKey -> Release -> PackageIndex -> m (Either SomeException [SourcePackage])
sourcePackagesOfIndex repo release index =
    case packageIndexArch index of
      Source -> liftIO (getPackages repo release index) >>= return . either Left (Right . List.map (toSourcePackage index . packageInfo))
      _ -> return (Right [])
