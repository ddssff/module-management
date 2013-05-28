{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing  #-}
module Debian.Repo.Package.GetPackages (getPackages) where
import Control.Exception as E (ErrorCall(ErrorCall), SomeException(..), try)
import qualified Data.ByteString.Lazy.Char8 as L (ByteString)
import Data.List as List (map)
import Debian.Apt.Index (Compression(..), controlFromIndex)
import qualified Debian.Control.Text as B (Control'(Control))
import Debian.Repo.Package.ToBinaryPackage (toBinaryPackage)
import Debian.Repo.PackageIndex (packageIndexPath)
import Debian.Repo.Types.PackageIndex (BinaryPackage, PackageIndex)
import Debian.Repo.Types.Release (Release)
import Debian.Repo.Types.Repo (RepoKey, repoKeyURI)
import Debian.URI (fileFromURIStrict)
import Network.URI (URI(uriPath))
import System.FilePath ((</>))

-- | Get the contents of a package index
getPackages :: RepoKey -> Release -> PackageIndex -> IO (Either SomeException [BinaryPackage])
getPackages repo release index =
    fileFromURIStrict uri' >>= readControl . either (Left . SomeException) Right
    where
      readControl :: Either SomeException L.ByteString -> IO (Either SomeException [BinaryPackage])
      readControl (Left e) = return (Left e)
      readControl (Right s) =
          try (case controlFromIndex Uncompressed (show uri') s of
                 Left e -> return $ Left (SomeException (ErrorCall (show uri' ++ ": " ++ show e)))
                 Right (B.Control control) -> return (Right $ List.map (toBinaryPackage release index) control)) >>=
          return . either (\ (e :: SomeException) -> Left . SomeException . ErrorCall . ((show uri' ++ ":") ++) . show $ e) id
      uri' = uri {uriPath = uriPath uri </> packageIndexPath release index}
      uri = repoKeyURI repo
      