{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.PutPackages
    ( putPackages
    ) where


import qualified Data.ByteString.Lazy.Char8 as L (fromChunks)
import Data.List as List (intercalate, intersperse, map)
import qualified Data.Text as T (concat, pack)
import Data.Text.Encoding (encodeUtf8)
import Debian.Control (formatParagraph)
import Debian.Repo.PackageIndex (packageIndexPath)
import Debian.Repo.Types.EnvPath (outsidePath)
import Debian.Repo.Types.PackageIndex (BinaryPackage(packageInfo), BinaryPackageLocal, PackageIndexLocal)
import Debian.Repo.Types.Release (Release)
import Debian.Repo.Types.Repository (LocalRepository, repoRoot)
import Extra.Files (writeAndZipFileWithBackup)
import System.FilePath ((</>))

putPackages :: LocalRepository -> Release -> PackageIndexLocal ->  [BinaryPackageLocal] -> IO ()
putPackages repo release index packages =
    writeAndZipFileWithBackup (outsidePath (repoRoot repo) </> packageIndexPath release index) (L.fromChunks [encodeUtf8 text]) >>= either (fail . intercalate "\n") return
    where
      text = T.concat (intersperse (T.pack "\n") . List.map formatParagraph . List.map packageInfo $ packages)

