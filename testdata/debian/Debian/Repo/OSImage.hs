{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.OSImage
    (
    -- * OSImage type
      OSImage(osRoot, osBaseDistro, osArch, osLocalMaster, osLocalCopy, osSourcePackageCache, osBinaryPackageCache)
    , createOSImage
    , cloneOSImage

    -- * OSImage Creation
    , pbuilder
    , debootstrap
    , syncOS'

    -- * OSImage Queries
    , osFullDistro
    , buildEssential

    -- * OSImage Manipulation
    , neuterEnv
    , restoreEnv
    , localeGen
    , removeEnv
    ) where

import Control.Exception (evaluate, SomeException)
import Control.Monad.Catch (try)
import Control.Monad.State (MonadIO(..))
import Data.Data (Data)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Debian.Arch (Arch)
import Debian.Pretty (ppDisplay)
import Debian.Relation (ParseRelations(parseRelations), Relations)
import Debian.Release (parseReleaseName, parseSection', ReleaseName(relName))
import Debian.Repo.EnvPath (EnvPath(EnvPath, envPath, envRoot), EnvRoot(rootPath), outsidePath)
import Debian.Repo.Internal.IO (buildArchOfRoot)
import Debian.Repo.LocalRepository (copyLocalRepo, LocalRepository)
import Debian.Repo.PackageIndex (BinaryPackage, SourcePackage)
import Debian.Repo.Prelude (isSublistOf, replaceFile, rsync, sameInode, sameMd5sum)
import Debian.Repo.Prelude.Verbosity (qPutStr, qPutStrLn, ePutStr, ePutStrLn, readProcFailing)
import Debian.Repo.Repo (repoKey, repoURI)
import Debian.Repo.Slice (NamedSliceList(sliceList), NamedSliceList(sliceListName), Slice(Slice, sliceRepoKey, sliceSource), SliceList(..))
import Debian.Sources (DebSource(..), DebSource(sourceDist, sourceUri), SourceType(..), SourceType(..))
import Debian.URI (uriToString')
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Posix.Files (createLink, deviceID, fileID, FileStatus, modificationTime)
import System.Process (shell)
import System.Process.Chunks (collectProcessTriple, collectProcessResult)
import System.Unix.Chroot (useEnv)
import System.Unix.Directory (removeRecursiveSafely)
import System.Unix.Mount (umountBelow)
import Text.Regex (matchRegex, mkRegex)

instance Ord FileStatus where
    compare a b = compare (deviceID a, fileID a, modificationTime a) (deviceID b, fileID b, modificationTime b)

instance Eq FileStatus where
    a == b = compare a b == EQ

-- |This type represents an OS image located at osRoot built from a
-- particular osBaseDistro using a particular osArch.  If an
-- osLocalRepo argument is given, that repository will be copied into
-- the environment and kept in sync, and lines will be added to
-- sources.list to point to it.
data OSImage
    = OS { osRoot :: EnvRoot
         , osBaseDistro :: NamedSliceList
         , osArch :: Arch
         , osLocalMaster :: LocalRepository
	 -- ^ The associated local repository, where packages we build
	 -- inside this image are first uploaded to.
         , osLocalCopy :: LocalRepository
	 -- ^ A copy of osLocalMaster located inside the os root environment.
         , osSourcePackageCache :: Maybe [SourcePackage]
         , osBinaryPackageCache :: Maybe [BinaryPackage]
         }

instance Ord OSImage where
    compare a b = case compare (osRoot a) (osRoot b) of
                    EQ -> case compare (osBaseDistro a) (osBaseDistro b) of
                            EQ -> compare (osArch a) (osArch b)
                            x -> x
                    x -> x

instance Eq OSImage where
    a == b = compare a b == EQ

-- |Create an OS image record
createOSImage ::
              EnvRoot			-- ^ The location where image is to be built
           -> NamedSliceList		-- ^ The sources.list of the base distribution
           -> LocalRepository           -- ^ The location of the local upload repository
           -> IO OSImage
createOSImage root distro repo =
    do copy <- copyLocalRepo (EnvPath {envRoot = root, envPath = "/work/localpool"}) repo
       -- At this point we can only support the build architecture of
       -- the underlying system.  We can support multiple
       -- distributions, but if the hardware is an amd64 the packages
       -- produced will be amd64.
       arch <- liftIO buildArchOfRoot
       let os = OS { osRoot = root
                   , osBaseDistro = distro
                   , osArch = arch
                   , osLocalMaster = repo
                   , osLocalCopy = copy
                   , osSourcePackageCache = Nothing
                   , osBinaryPackageCache = Nothing }
       return os

-- | Create the OSImage record for a copy of an existing OSImage at a
-- different location.
cloneOSImage :: OSImage -> EnvRoot -> IO OSImage
cloneOSImage src dst = do
  copy <- copyLocalRepo (EnvPath {envRoot = dst, envPath = "/work/localpool"}) (osLocalMaster src)
  return $ src {osRoot = dst, osLocalCopy = copy}

-- | Set the location of the OSImage's root directory - where you
-- would cd to before running chroot.
-- chrootEnv :: OSImage -> EnvRoot -> OSImage
-- chrootEnv os dst = os {osRoot = dst}

-- The following are path functions which can be used while
-- constructing instances of AptCache.  Each is followed by a
-- corresponding function that gives the same result when applied to
-- an AptCache instance.

data SourcesChangedAction =
    SourcesChangedError |
    UpdateSources |
    RemoveRelease
    deriving (Eq, Show, Data, Typeable)

instance Show OSImage where
    show os = intercalate " " ["OS {",
                               rootPath (osRoot os),
                               relName (sliceListName (osBaseDistro os)),
                               show (osArch os),
                               show (osLocalCopy os)]

-- |The sources.list is the list associated with the distro name, plus
-- the local sources where we deposit newly built packages.
osFullDistro :: OSImage -> SliceList
osFullDistro os =
    let base = osBaseDistro os
        repo' = osLocalCopy os
        name = relName (sliceListName base)
        localSources :: SliceList
        localSources = SliceList {slices = [Slice {sliceRepoKey = repoKey repo', sliceSource = src},
                                            Slice {sliceRepoKey = repoKey repo', sliceSource = bin}]}
        src = DebSource Deb (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"]))
        bin = DebSource DebSrc (repoURI repo') (Right (parseReleaseName name, [parseSection' "main"])) in
    SliceList { slices = slices (sliceList base) ++ slices localSources }

data UpdateError
    = Changed ReleaseName FilePath SliceList SliceList
    | Missing ReleaseName FilePath
    | Flushed

instance Show UpdateError where
    show (Changed r p l1 l2) = unwords ["Changed", show r, show p, ppDisplay l1, ppDisplay l2]
    show (Missing r p) = unwords ["Missing", show r, show p]
    show Flushed = "Flushed"

syncOS' :: OSImage -> EnvRoot -> IO OSImage
syncOS' src dst = do
  mkdir
  umount
  (_result, _, _) <- rsync ["--exclude=/work/build/*"] (rootPath (osRoot src)) (rootPath dst)
  cloneOSImage src dst
    where
      mkdir = createDirectoryIfMissing True (rootPath dst ++ "/work")
      umount =
          do srcResult <- umountBelow False (rootPath (osRoot src))
             dstResult <- umountBelow False (rootPath dst)
             case filter (\ (_, (code, _, _)) -> code /= ExitSuccess) (srcResult ++ dstResult) of
               [] -> return ()
               failed -> fail $ "umount failure(s): " ++ show failed

-- | FIXME - we should notice the locale problem and run this.
localeGen :: OSImage -> String -> IO ()
localeGen os locale =
    do let root = osRoot os
       qPutStr ("Generating locale " ++  locale ++ " (" ++ stripDist (rootPath root) ++ ")...")
       result <- try $ useEnv (rootPath root) forceList (readProcFailing (shell cmd) "")
       case result of
         Left (e :: SomeException) -> error $ "Failed to generate locale " ++ rootPath root ++ ": " ++ show e
         Right output ->
             case collectProcessTriple output of
               (ExitSuccess, _, _) -> qPutStrLn "done"
               (code, _, _) -> error $ "Failed to generate locale " ++ rootPath root ++ ": " ++ cmd ++ " -> " ++ show code
    where
      cmd = "locale-gen " ++ locale


-- |To "neuter" an executable is to replace it with a hard link to
-- \/bin\/true in such a way that the operation can be reversed.  This
-- is done in order to make it safe to install files into it when it
-- isn't "live".  If this operation fails it is assumed that the
-- image is damaged, so it is removed.
neuterEnv :: OSImage -> IO ()
neuterEnv os =
    do qPutStr ("Neutering OS image (" ++ stripDist root ++ ")...")
       result <- try $ mapM_ (neuterFile os) neuterFiles
       either (\ (e :: SomeException) -> error $ "Failed to neuter environment " ++ root ++ ": " ++ show e)
              (\ _ -> qPutStrLn "done.")
              result
    where
      root = rootPath (osRoot os)

neuterFiles :: [(FilePath, Bool)]
neuterFiles = [("/sbin/start-stop-daemon", True),
	       ("/usr/sbin/invoke-rc.d", True),
	       ("/sbin/init",False),
	       ("/usr/sbin/policy-rc.d", False)]

-- neuter_file from build-env.ml
neuterFile :: OSImage -> (FilePath, Bool) -> IO ()
neuterFile os (file, mustExist) =
    do
      -- putStrBl ("Neutering file " ++ file)
      exists <- doesFileExist (outsidePath fullPath)
      if exists then
          neuterExistantFile else
          if mustExist then
              error ("Can't neuter nonexistant file: " ++ outsidePath fullPath) else
              return () -- putStrBl "File doesn't exist, nothing to do"

    where
      neuterExistantFile =
          do
            sameFile <- sameInode (outsidePath fullPath) (outsidePath binTrue)
            if sameFile then
                return () else -- putStrBl "File already neutered"
                neuterUnneuteredFile
      neuterUnneuteredFile =
          do
            hasReal <- doesFileExist (outsidePath fullPath ++ ".real")
            if hasReal then
                neuterFileWithRealVersion else
                neuterFileWithoutRealVersion
            createLink (outsidePath binTrue) (outsidePath fullPath)
      neuterFileWithRealVersion =
          do
            sameCksum <- sameMd5sum (outsidePath fullPath) (outsidePath fullPath ++ ".real")
            if sameCksum then
                removeFile (outsidePath fullPath) else
                error (file ++ " and " ++ file ++ ".real differ (in " ++ rootPath root ++ ")")

      neuterFileWithoutRealVersion = renameFile (outsidePath fullPath) (outsidePath fullPath ++ ".real")

      fullPath = EnvPath root file
      binTrue = EnvPath root "/bin/true"
      root = osRoot os

-- |Reverse the neuterEnv operation.
restoreEnv :: OSImage -> IO OSImage
restoreEnv os =
    do
      qPutStr "De-neutering OS image..."
      result <- try $ mapM_ (restoreFile os) neuterFiles
      either (\ (e :: SomeException) -> error $ "damaged environment " ++ rootPath root ++ ": " ++ show e ++ "\n  please remove it.")
                 (\ _ -> return os) result
    where
      root = osRoot os

-- check_and_restore from build-env.ml
restoreFile :: OSImage -> (FilePath, Bool) -> IO ()
restoreFile os (file, mustExist) =
    do
      exists <- doesFileExist (outsidePath fullPath)
      if exists then
          restoreExistantFile else
          if mustExist then
              error ("Can't restore nonexistant file: " ++ outsidePath fullPath) else
              return ()
    where
      restoreExistantFile =
          do
            isTrue <- sameInode (outsidePath fullPath) (outsidePath binTrue)
            hasReal <- doesFileExist (outsidePath fullPath ++ ".real")
            case (isTrue, hasReal) of
              (True, True) ->
                  do
                    removeFile (outsidePath fullPath)
                    renameFile (outsidePath fullPath ++ ".real") (outsidePath fullPath)
              (False, _) -> error "Can't restore file not linked to /bin/true"
              (_, False) -> error "Can't restore file with no .real version"

      fullPath = EnvPath root file
      binTrue = EnvPath root "/bin/true"
      root = osRoot os

-- | Build the dependency relations for the build essential packages.
-- For this to work the @build-essential@ package must be installed in
-- the OSImage.
buildEssential :: OSImage -> IO Relations
buildEssential os = do
      let root = osRoot os
      -- qPutStrLn "Computing build essentials"
      essential <-
          readFile (rootPath root ++ "/usr/share/build-essential/essential-packages-list") >>=
          return . lines >>= return . dropWhile (/= "") >>= return . tail >>= return . filter (/= "sysvinit") >>=
          return . parseRelations . (intercalate ", ") >>=
          return . (either (error "parse error in /usr/share/build-essential/essential-packages-list") id)
      let re = mkRegex "^[^ \t]"
      relationText <-
          readFile (rootPath root ++ "/usr/share/build-essential/list") >>=
          return . lines >>=
          return . dropWhile (/= "BEGIN LIST OF PACKAGES") >>= return . tail >>=
          return . takeWhile (/= "END LIST OF PACKAGES") >>=
          return . filter ((/= Nothing) . (matchRegex re))
      -- ePut ("buildEssentialText: " ++ intercalate ", " relationText)
      let buildEssential'' = parseRelations (intercalate ", " relationText)
      let buildEssential' = either (\ l -> error ("parse error in /usr/share/build-essential/list:\n" ++ show l)) id buildEssential''
      return (essential ++ buildEssential')

-- |Remove an image.  The removeRecursiveSafely function is used to
-- ensure that any file systems mounted inside the image are unmounted
-- and not destroyed.
removeEnv :: OSImage -> IO ()
removeEnv os =
    do
      ePutStr "Removing build environment..."
      removeRecursiveSafely (rootPath root)
      ePutStrLn "done."
    where
      root = osRoot os

-- prefixes :: Maybe (L.ByteString, L.ByteString)
-- prefixes = Just (" 1> ", " 2> ")

stripDist :: FilePath -> FilePath
stripDist path = maybe path (\ n -> drop (n + 7) path) (isSublistOf "/dists/" path)

-- | This is a deepseq thing
forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output

pbuilder :: FilePath
         -> EnvRoot
         -> NamedSliceList
         -> LocalRepository
         -> [String]
         -> [String]
         -> [String]
         -> IO OSImage
pbuilder top root distro repo _extraEssential _omitEssential _extra =
      -- We can't create the environment if the sources.list has any
      -- file:// URIs because they can't yet be visible inside the
      -- environment.  So we grep them out, create the environment, and
      -- then add them back in.
    do ePutStrLn ("Creating clean build environment (" ++ relName (sliceListName distro) ++ ")")
       ePutStrLn ("# " ++ cmd top)
       let codefn ExitSuccess = return ()
           codefn failure = error ("Could not create build environment:\n " ++ cmd top ++ " -> " ++ show failure)
       liftIO (readProcFailing (shell (cmd top)) "") >>= return . collectProcessTriple >>= \ (result, _, _) -> codefn result
       ePutStrLn "done."
       os <- createOSImage root distro repo -- arch?  copy?
       let sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
       -- Rewrite the sources.list with the local pool added.
           sources = ppDisplay $ osFullDistro os
       replaceFile sourcesPath' sources
       return os
    where
      cmd x =
          intercalate " " $ [ "pbuilder"
                            , "--create"
                            , "--distribution", (relName . sliceListName $ distro)
                            , "--basetgz", x </> "pbuilderBase"
                            , "--buildplace", rootPath root
                            , "--preserve-buildplace"
                            ]

-- Create a new clean build environment in root.clean
-- FIXME: create an ".incomplete" flag and remove it when build-env succeeds
debootstrap
    :: EnvRoot
    -> NamedSliceList
    -> LocalRepository
    -> [String]
    -> [String]
    -> [String]
    -> IO OSImage
debootstrap root distro repo include exclude components =
    do
      ePutStr (unlines [ "Creating clean build environment (" ++ relName (sliceListName distro) ++ ")"
                       , "  root: " ++ show root
                       , "  baseDist: " ++ show baseDist
                       , "  mirror: " ++ show mirror ])
      -- We can't create the environment if the sources.list has any
      -- file:// URIs because they can't yet be visible inside the
      -- environment.  So we grep them out, create the environment, and
      -- then add them back in.
      readProcFailing (shell cmd) "" >>= codefn . fst . collectProcessResult
      ePutStrLn "done."
      os <- createOSImage root distro repo -- arch?  copy?
      let sourcesPath' = rootPath root ++ "/etc/apt/sources.list"
      -- Rewrite the sources.list with the local pool added.
          sources = ppDisplay $ osFullDistro os
      liftIO $ replaceFile sourcesPath' sources
      return os
    where
      codefn ExitSuccess = return ()
      codefn failure = error ("Could not create build environment:\n " ++ cmd ++ " -> " ++ show failure)

      woot = rootPath root
      wootNew = woot ++ ".new"
      baseDist = either id (relName . fst) . sourceDist . sliceSource . head . slices . sliceList $ distro
      mirror = uriToString' . sourceUri . sliceSource . head . slices . sliceList $ distro
      cmd = intercalate " && "
              ["set -x",
               "rm -rf " ++ wootNew,
               ("debootstrap " ++
                (if include /= [] then "--include=" ++ intercalate "," include ++ " " else "") ++
                (if exclude /= [] then "--exclude=" ++ intercalate "," exclude ++ " " else "") ++
                "--variant=buildd " ++
                "--components=" ++ intercalate "," components ++ " " ++
                baseDist ++ " " ++
                wootNew ++ " " ++
                mirror),
               "cat " ++ wootNew ++ "/etc/apt/sources.list | sed -e 's/^deb /deb-src /' >>" ++ wootNew ++ "/etc/apt/sources.list",
               "mkdir -p " ++ woot,
               "rm -rf " ++ woot,
               "mv " ++ wootNew ++ " " ++ woot]
