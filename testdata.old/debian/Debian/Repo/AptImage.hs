{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings,
             PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.AptImage
    ( aptDir
    , buildArchOfRoot
    , aptGetSource
    , aptGetUpdate
    ) where

import Control.Applicative ((<$>))
import Control.Category ((.))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import Debian.Pretty (ppDisplay)
import Debian.Relation (PkgName, SrcPkgName(unSrcPkgName))
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import Debian.Repo.Internal.Apt (AptImage(aptImageRoot, aptImageSources), MonadApt(getApt))
import Debian.Repo.Prelude.Verbosity (readProcFailing)
import Debian.Repo.Slice (NamedSliceList(sliceListName))
import Debian.Repo.Top (distDir, MonadTop)
import Debian.Version (DebianVersion, prettyDebianVersion)
import Prelude hiding ((.))
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (CreateProcess(cwd), proc, readProcessWithExitCode)

-- | The location of the top directory of a source packages's files in
-- an AptImage (but not an OSImage.)
aptDir :: (MonadTop m, MonadApt m) => SrcPkgName -> m FilePath
aptDir package =
    do rel <- aptImageSources <$> getApt
       dir <- distDir (sliceListName rel)
       return $ dir </> "apt" </> unSrcPkgName package

-- The following are path functions which can be used while
-- constructing instances of AptCache.  Each is followed by a
-- corresponding function that gives the same result when applied to
-- an AptCache instance.

buildArchOfRoot :: IO Arch
buildArchOfRoot =
    do a@(code1, out1, _err1) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
       b@(code2, out2, _err2) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
       case (code1, lines out1, code2, lines out2) of
         (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
             return $ Binary (parseArchOS os) (parseArchCPU cpu)
         _ -> error $ "Failure computing build architecture of /: " ++ show (a, b)
    where
      parseArchOS "any" = ArchOSAny
      parseArchOS x = ArchOS x
      parseArchCPU "any" = ArchCPUAny
      parseArchCPU x = ArchCPU x

data SourcesChangedAction =
    SourcesChangedError |
    UpdateSources |
    RemoveRelease
    deriving (Eq, Show, Data, Typeable)

-- | Run an apt-get command in a particular directory with a
-- particular list of packages.  Note that apt-get source works for
-- binary or source package names.
aptGetSource :: (MonadIO m, MonadApt m, PkgName n) => FilePath -> [(n, Maybe DebianVersion)] -> m ()
aptGetSource dir packages =
    do args <- aptOpts
       let p = (proc "apt-get" (args ++ ["source"] ++ map formatPackage packages)) {cwd = Just dir}
       liftIO $ createDirectoryIfMissing True dir >> readProcFailing p "" >> return ()
    where
      formatPackage (name, Nothing) = ppDisplay name
      formatPackage (name, Just version) = ppDisplay name ++ "=" ++ show (prettyDebianVersion version)

aptGetUpdate :: (MonadIO m, MonadApt m) => m ()
aptGetUpdate =
    do args <- aptOpts
       let p = (proc "apt-get" (args ++ ["update"]))
       _ <- liftIO $ readProcFailing p ""
       return ()

aptOpts :: MonadApt m => m [String]
aptOpts =
    do root <- (rootPath . aptImageRoot) <$> getApt
       return $ [ "-o=Dir::State::status=" ++ root ++ "/var/lib/dpkg/status"
                , "-o=Dir::State::Lists=" ++ root ++ "/var/lib/apt/lists"
                , "-o=Dir::Cache::Archives=" ++ root ++ "/var/cache/apt/archives"
                , "-o=Dir::Etc::SourceList=" ++ root ++ "/etc/apt/sources.list"
                , "-o=Dir::Etc::SourceParts=" ++ root ++ "/etc/apt/sources.list.d" ]
