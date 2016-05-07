-- | A repository located on localhost
{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, StandaloneDeriving, ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.LocalRepository
    ( LocalRepository(..)
    , Layout(..)
    , poolDir'
    , readLocalRepo
    , copyLocalRepo -- repoCD
    , setRepositoryCompatibility
    , verifyUploadURI
    , uploadRemote
    , uploadLocal
    ) where

import Control.Applicative ((<$>))
import Control.Applicative.Error (Failing(Success, Failure))
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy as L (ByteString, empty)
import Data.List (groupBy, isPrefixOf, partition, sort, sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Set as Set (fromList, member)
import Data.Text as T (unpack)
import Data.Time (NominalDiffTime)
import Debian.Arch (Arch, parseArch)
import Debian.Changes (ChangedFileSpec(changedFileName, changedFileSection), ChangesFile(changeDir, changeFiles, changeInfo, changePackage, changeRelease, changeVersion))
import qualified Debian.Control.Text as S (Control'(Control), ControlFunctions(parseControlFromFile), fieldValue)
import qualified Debian.Control.Text as T (fieldValue)
import Debian.Pretty (PP(..), ppDisplay)
import Debian.Release (parseReleaseName, ReleaseName(..), releaseName', Section(..), sectionName', SubSection(section))
import Debian.Repo.Changes (changeKey, changePath, findChangesFiles)
import Debian.Repo.EnvPath (EnvPath(envPath), outsidePath)
import Debian.Repo.Fingerprint (readUpstreamFingerprint)
import Debian.Repo.Prelude (cond, maybeWriteFile, partitionM, replaceFile, rsync)
import Debian.Repo.Prelude.SSH (sshVerify)
import Debian.Repo.Prelude.Verbosity (qPutStrLn, timeTask, readProcFailing)
import Debian.Repo.Release (parseReleaseFile, Release)
import Debian.Repo.Repo (compatibilityFile, libraryCompatibilityLevel, Repo(..), RepoKey(..))
import Debian.URI (URI(uriAuthority, uriPath), URIAuth(uriPort, uriRegName, uriUserInfo), uriToString')
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import Network.URI (URI(..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.Exit (ExitCode(ExitFailure), ExitCode(ExitSuccess))
import System.FilePath ((</>), splitFileName)
import qualified System.Posix.Files as F (createLink, getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink, removeLink)
import System.Process (readProcessWithExitCode, CreateProcess(cwd, cmdspec), showCommandForUser, proc)
import System.Process.Chunks (Chunk(..), collectProcessResult, showCmdSpecForUser)
import Text.Regex (matchRegex, mkRegex)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

data LocalRepository
    = LocalRepository
      { repoRoot :: EnvPath
      , repoLayout :: (Maybe Layout)
      , repoReleaseInfoLocal :: [Release]
      } deriving (Read, Show, Ord, Eq)

instance Pretty (PP LocalRepository) where
    pPrint (PP (LocalRepository root _ _)) =
        text $ show $ URI { uriScheme = "file:"
                          , uriAuthority = Nothing
                          , uriPath = envPath root
                          , uriQuery = ""
                          , uriFragment = "" }

-- |The possible file arrangements for a repository.  An empty
-- repository does not yet have either of these attributes.
data Layout = Flat | Pool deriving (Eq, Ord, Read, Show, Bounded, Enum)

instance Repo LocalRepository where
    repoKey (LocalRepository path _ _) = Local path -- fromJust . parseURI $ "file://" ++ envPath path
    repoReleaseInfo (LocalRepository _ _ info) = info

-- | Return the subdirectory where a source package with the given
-- section and name would be installed given the layout of the
-- repository.
poolDir :: LocalRepository -> Section -> String -> FilePath
poolDir r section' source =
    case repoLayout r of
      Just Pool ->
          "pool/" ++ sectionName' section' </> prefixDir </> source
              where prefixDir =
                        if isPrefixOf "lib" source
                        then take (min 4 (length source)) source
                        else take (min 1 (length source)) source
      _ -> ""

-- | Return the subdirectory in the pool where a source package would be
-- installed.
poolDir' :: LocalRepository -> ChangesFile -> ChangedFileSpec -> FilePath
poolDir' repo changes file =
    case T.fieldValue "Source" (changeInfo changes) of
      Nothing -> error "No 'Source' field in .changes file"
      Just source -> poolDir repo (section . changedFileSection $ file) (unpack source)

readLocalRepo :: MonadIO m => EnvPath -> Maybe Layout -> m (Maybe LocalRepository)
readLocalRepo root layout =
    do names <- liftIO (getDirectoryContents distDir) >>= return . filter (\ x -> not . elem x $ [".", ".."])
       (links, dists) <- partitionM (liftIO . isSymLink . (distDir </>)) names
       linkText <- mapM (liftIO . F.readSymbolicLink) (map (distDir </>) links)
       let aliasPairs = zip linkText links ++ map (\ dist -> (dist, dist)) dists
       let distGroups = groupBy fstEq . sort $ aliasPairs
       let aliases = map (checkAliases  . partition (uncurry (==))) distGroups
       releaseInfo <- mapM (liftIO . getReleaseInfo) aliases
       case releaseInfo of
         [] -> return Nothing
         _ -> return $ Just $ LocalRepository { repoRoot = root
                                              , repoLayout = layout
                                              , repoReleaseInfoLocal = releaseInfo }
    where
      fstEq (a, _) (b, _) = a == b
      checkAliases :: ([(String, String)], [(String, String)]) -> (ReleaseName, [ReleaseName])
      checkAliases ([(realName, _)], aliases) = (parseReleaseName realName, map (parseReleaseName . snd) aliases)
      checkAliases _ = error "Symbolic link points to itself!"
      getReleaseInfo :: (ReleaseName, [ReleaseName]) -> IO Release
      getReleaseInfo (dist, aliases) = parseReleaseFile (releasePath dist) dist aliases
      releasePath dist = distDir </> releaseName' dist </> "Release"
      distDir = outsidePath root </> "dists"

isSymLink :: FilePath -> IO Bool
isSymLink path = F.getSymbolicLinkStatus path >>= return . F.isSymbolicLink

-- |Change the root directory of a repository.  FIXME: This should
-- also sync the repository to ensure consistency.
-- repoCD :: EnvPath -> LocalRepository -> LocalRepository
-- repoCD path repo = repo { repoRoot_ = path }

copyLocalRepo :: MonadIO m => EnvPath -> LocalRepository -> m LocalRepository
copyLocalRepo dest repo =
    do liftIO $ createDirectoryIfMissing True (outsidePath dest)
       (result :: (ExitCode, String, String)) <- liftIO $ rsync [] (outsidePath (repoRoot repo)) (outsidePath dest)
       case result of
         (ExitSuccess, _, _) -> return $ repo {repoRoot = dest}
         code -> error $ "*** FAILURE syncing local repository " ++ src ++ " -> " ++ dst ++ ": " ++ show code
    where
      src = outsidePath (repoRoot repo)
      dst = outsidePath dest

-- | Create or update the compatibility level file for a repository.
setRepositoryCompatibility :: LocalRepository -> IO ()
setRepositoryCompatibility r =
    maybeWriteFile path (show libraryCompatibilityLevel ++ "\n")
    where path = outsidePath (repoRoot r) </> compatibilityFile

-- |The file produced by dupload when a package upload attempt is made.
data UploadFile = Upload FilePath String DebianVersion Arch

-- |Make sure we can access the upload uri without typing a password.
verifyUploadURI :: MonadIO m => Bool -> URI -> m ()
verifyUploadURI doExport uri = do
  qPutStrLn ("Verifying upload URI: " ++ show uri)
  case doExport of
    True -> export
    False -> verify >> mkdir
    where
      export =
          do -- The code in sshExport needs to be rewritten.
             -- liftIO $ uncurry sshExport (uriDest uri) >>= either fail return
             verify
             mkdir
      verify =
          do result <- liftIO $ uncurry sshVerify (uriDest uri)
             case result of
               Right () -> return ()
               Left s -> error $ "Unable to reach " ++ uriToString' uri ++ ": " ++ s
             mkdir
      mkdir =
          case uriAuthority uri of
            Nothing -> error $ "Internal error 7"
            Just auth ->
                do let cmd = "ssh"
                       args = [uriUserInfo auth ++ uriRegName auth ++ uriPort auth,
                               "mkdir", "-p", uriPath uri ++ "/incoming"]
                   (result, _, _) <- liftIO (readProcessWithExitCode cmd args "")
                   case result of
                     ExitSuccess -> return ()
                     _ -> fail $ showCommandForUser cmd args ++ " -> " ++ show result

uriDest :: URI -> ([Char], Maybe Int)
uriDest uri =
    (uriUserInfo auth ++ uriRegName auth, port)
    where
      auth = maybe (error "Internal error 8") id (uriAuthority uri)
      port =
          case uriPort auth of
            (':' : number) -> Just (read number)
            "" -> Nothing
            x -> error $ "Internal error 9: invalid port " ++ x

-- | Upload all the packages in a local repository to a the incoming
-- directory of a remote repository (using dupload.)
uploadRemote :: LocalRepository		-- ^ Local repository holding the packages.
             -> URI			-- ^ URI of upload repository
             -> IO [Failing ([Chunk L.ByteString], NominalDiffTime)]
uploadRemote repo uri =
    do let dir = (outsidePath root)
       changesFiles <- findChangesFiles (outsidePath root)
       let changesFileGroups = map (sortBy compareVersions) . groupByNameAndDist $ changesFiles
       let newestChangesFiles = catMaybes (map listToMaybe changesFileGroups)
       -- hPutStrLn stderr $ "Newest: " ++ show newestChangesFiles
       uploaded <- (Set.fromList .
                    map (\ [_, name', version, arch] -> (name', parseDebianVersion version, parseArch arch)) .
                    catMaybes .
                    map (matchRegex (mkRegex "^(.*/)?([^_]*)_(.*)_([^.]*)\\.upload$"))) <$> getDirectoryContents dir
       let (readyChangesFiles, uploadedChangesFiles) = partition (\ f -> not . Set.member (changeKey f) $ uploaded) newestChangesFiles
       -- hPutStrLn stderr $ "Uploaded: " ++ show uploadedChangesFiles
       -- hPutStrLn stderr $ "Ready: " ++ show readyChangesFiles
       validChangesFiles <- mapM validRevision' readyChangesFiles
       -- hPutStrLn stderr $ "Valid: " ++ show validChangesFiles
       mapM dupload' validChangesFiles
    where
      keepNewest (Success newest : older) =
          Success newest : map tooOld older
      keepNewest xs = xs
      -- Add to Control.Applicative.Error
      -- partitionFailing :: [Failing a] -> ([[String]], [a])
      -- partitionFailing = foldr f ([], []) where f (Failure ms) (msgs, xs) = (ms : msgs, xs)
      --                                           f (Success x) (msgs, xs) = (msgs, x : xs)
      tooOld (Failure x) = Failure x
      tooOld (Success x) = Failure ["Not the newest version in incoming: " ++ ppDisplay x]
      successes (Success x : xs) = x : successes xs
      successes (Failure _ : xs) = successes xs
      successes [] = []
      root = repoRoot repo
{-
      rejectOlder :: ([ChangesFile], [(ChangesFile, String)]) ->  ([ChangesFile], [(ChangesFile, String)])
      rejectOlder (accept, reject) =
          (accept', (map tag reject' ++ reject))
          where accept' = map head sortedGroups
                reject' = concat . map tail $ sortedGroups
                sortedGroups = map (sortBy compareVersions) (groupByNameAndDist accept)
                tag x = (x, "Not the newest version in incoming")
-}
      compareVersions a b = compare (changeVersion b) (changeVersion a)
      groupByNameAndDist :: [ChangesFile] -> [[ChangesFile]]
      groupByNameAndDist = groupBy equalNameAndDist . sortBy compareNameAndDist
      equalNameAndDist a b = compareNameAndDist a b == EQ
      compareNameAndDist a b =
          case compare (changePackage a) (changePackage b) of
            EQ -> compare (changeRelease a) (changeRelease b)
            x -> x
      --showReject (changes, tag) = Debian.Repo.Changes.name changes ++ ": " ++ tag
      dupload' (Failure x) = return (Failure x)
      dupload' (Success c) = liftIO (dupload uri (outsidePath root) (changePath c))

validRevision' :: ChangesFile -> IO (Failing ChangesFile)
validRevision' c = validRevision
    where
      validRevision :: IO (Failing ChangesFile)
      validRevision =
          doesFileExist dscPath >>=
                        cond (S.parseControlFromFile dscPath >>=
                              either (\ e -> return (Failure [show e])) (checkRevision dscPath))
                             (return (Success c))
      dscPath = changeDir c </> changePackage c ++ "_" ++ show (prettyDebianVersion (changeVersion c)) ++ ".dsc"
      checkRevision :: FilePath -> S.Control' String -> IO (Failing ChangesFile)
      checkRevision dscPath' (S.Control [p]) =
          case maybe (Failure ["Missing Fingerprint field in " ++ dscPath'])
                     (\ s -> maybe (Failure ["Parse error in revision string: " ++  show s]) Success (readUpstreamFingerprint s))
                     (S.fieldValue "Fingerprint" p) of
            Failure msgs -> return (Failure msgs)
            -- Success x | x == invalidRevision -> return (Failure ["Invalid revision: " ++ show x])
            Success _ -> return (Success c)
      checkRevision dscPath' _ = return (Failure ["Invalid .dsc file: " ++ show dscPath'])
      -- invalidRevision = "none"
      -- Parse the "Fingerprint:" value describing the origin of the
      -- package's source and the dependency versions used to build it:
      --   Revision: <revisionstring> dep1=ver1 dep2=ver2 ...
      -- parseRevision :: String -> Failing (String, [PackageID BinPkgName])
      -- parseRevision s =
      --     case reads s :: [(String, String)] of
      --       [(method, etc)] ->
      --           case words etc of
      --             (sourceVersion : buildDeps)
      --               | not (elem '=' sourceVersion) ->
      --                   Success (method, map readSimpleRelation buildDeps)
      --             buildDeps ->
      --                   Success (method, map readSimpleRelation buildDeps)
      --       _ -> Failure ["Invalid revision field: " ++ s]

uploadKey :: UploadFile -> (String, DebianVersion, Arch)
uploadKey (Upload _ name ver arch) = (name, ver, arch)

-- |Parse the name of a .upload file
parseUploadFilename :: FilePath
                    -> String
                    -> Failing UploadFile
parseUploadFilename dir name =
    case matchRegex (mkRegex "^(.*/)?([^_]*)_(.*)_([^.]*)\\.upload$") name of
      Just [_, name', version, arch] -> Success (Upload dir name' (parseDebianVersion version) (parseArch arch))
      _ -> Failure ["Invalid .upload file name: " ++ name]

{-
accept :: (a -> Bool) -> (a -> (a, String)) -> ([a], [(a, String)]) -> ([a], [(a, String)])
accept p tag (accepted, rejected) =
    (accepted', map tag rejected' ++ rejected)
    where (accepted', rejected') = partition p accepted

acceptM :: (Monad m) => (a -> m Bool) -> (a -> (a, String)) -> ([a], [(a, String)]) -> m ([a], [(a, String)])
acceptM p tag (accept, reject) =
    do (accept', reject') <- partitionM p accept
       return (accept', (map tag reject' ++ reject))
-}

-- |Run dupload on a changes file with an optional host (--to)
-- argument.
dupload :: URI		-- user
        -> FilePath	-- The directory containing the .changes file
        -> String	-- The name of the .changes file to upload
        -> IO (Failing ([Chunk L.ByteString], NominalDiffTime))
dupload uri dir changesFile  =
    case uriAuthority uri of
      Nothing -> error ("Invalid Upload-URI: " ++ uriToString' uri)
      Just auth -> do
        let config = ("package config;\n" ++
                      "$cfg{'default'} = {\n" ++
                      "        fqdn => \"" ++ uriRegName auth ++ uriPort auth ++ "\",\n" ++
                      "        method => \"scpb\",\n" ++
	              "        login => \"" ++ init (uriUserInfo auth) ++ "\",\n" ++
                      "        incoming => \"" ++ uriPath uri ++ "/incoming\",\n" ++
                      "        dinstall_runs => 1,\n" ++
                      "};\n\n" ++
		      "$preupload{'changes'} = '';\n\n" ++
                      "1;\n")
        replaceFile (dir ++ "/dupload.conf") config
        let cmd = (proc "dupload" ["--to", "default", "-c", changesFile]) {cwd = Just dir}
        qPutStrLn ("Uploading " ++ show changesFile)
        (chunks, elapsed) <- timeTask $ readProcFailing cmd L.empty
        qPutStrLn ("Upload finished, elapsed time " ++ show elapsed)
        let (code, out) = collectProcessResult chunks
        case code of
          ExitFailure _ ->
              do let message = "dupload in " ++ dir ++ " failed: " ++ showCmdSpecForUser (cmdspec cmd) ++ " -> " ++ show code
                 qPutStrLn message
                 return $ Failure [message]
          ExitSuccess -> return $ Success (out, elapsed)

ignore :: forall a. IO (Either String [Chunk L.ByteString]) -> a -> IO (Either String [Chunk L.ByteString])
ignore result _ = result

-- | Move a build result into a local repository's 'incoming' directory.
uploadLocal :: LocalRepository -> ChangesFile -> IO ()
uploadLocal repo changesFile =
    do let paths = map (\ file -> changeDir changesFile </> changedFileName file) (changeFiles changesFile)
       mapM_ (liftIO . install (outsidePath root)) (changePath changesFile : paths)
    where
      root = repoRoot repo
      -- Hard link a file into the incoming directory
      install root' path =
	  do removeIfExists (dest root' path)
	     F.createLink path (dest root' path)
             -- F.removeLink path
      dest root' path = root' ++ "/incoming/" ++ snd (splitFileName path)
      removeIfExists path =
	  do exists <- doesFileExist path
	     if exists then F.removeLink path  else return ()
