{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Repository
    ( Repository
    , LocalRepository, repoRoot, repoLayout, repoReleaseInfoLocal
    , RemoteRepository
    , Layout(..)
    , MonadRepoCache(getRepoCache, putRepoCache)
    , loadRepoCache
    , saveRepoCache
    , fromLocalRepository
    , readLocalRepo
    , prepareLocalRepository
    , copyLocalRepo -- repoCD
    , prepareRepository
    , setRepositoryCompatibility
    , flushLocalRepository
    , poolDir'
    -- , sliceReleaseNames
    , repoReleaseNames
    ) where

import Control.Applicative.Error (Failing(Success, Failure), maybeRead)
import Control.Exception (ErrorCall(..), SomeException, toException)
import Control.Monad (filterM, unless, when)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.List (groupBy, intercalate, isPrefixOf, partition, sort)
import Data.Map as Map (empty, fromList, insert, insertWith, lookup, Map, toList, union)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text as T (Text, unpack)
import Debian.Changes (ChangedFileSpec(changedFileSection), ChangesFile(changeInfo))
import qualified Debian.Control.Text as T (Control'(Control), ControlFunctions(parseControl), fieldValue, Paragraph, Paragraph')
import Debian.Release (parseReleaseName, ReleaseName(..), releaseName', Section, sectionName', SubSection(section))
import Debian.Repo.Monads.Top (MonadTop, sub)
import Debian.Repo.Sync (rsync)
import Debian.Repo.Types.Common (compatibilityFile, EnvPath(EnvPath), EnvRoot(EnvRoot), libraryCompatibilityLevel, outsidePath, Repo(..), RepoKey(..))
import Debian.Repo.Types.Release (makeReleaseInfo, Release(releaseName))
import Debian.URI (dirFromURI, fileFromURI, fromURI', toURI', URI(uriScheme, uriPath), URI', uriToString')
import Debian.UTF8 as Deb (decode)
import Extra.Files (maybeWriteFile)
import Extra.List (partitionM)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Posix.Files as F (fileMode, getFileStatus, getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink, removeLink, setFileMode)
import System.Process.Progress (qPutStr, qPutStrLn, quieter)
import System.Unix.Directory (removeRecursiveSafely)
import qualified Text.Format as F (Pretty(..))
import Text.PrettyPrint.ANSI.Leijen (text)
import Text.Regex (matchRegex, mkRegex)
import qualified Tmp.File as F (File(..), readFile, Source(RemotePath))

--------------------- REPOSITORY -----------------------

-- | The Repository type reprents any instance of the Repo class, so
-- it might be local or remote.
--data Repository = forall a. (Repo a) => Repository a
data Repository
    = LocalRepo LocalRepository
    | RemoteRepo RemoteRepository
    deriving (Show, Read)

instance Ord Repository where
    compare a b = compare (repoKey a) (repoKey b)

instance Eq Repository where
    a == b = compare a b == EQ

-- | URI has a bogus show function, which we are using here.
instance F.Pretty URI' where
    pretty = text . show . fromURI'

instance F.Pretty Repository where
    pretty (LocalRepo r) = text $ outsidePath (repoRoot r)
    pretty (RemoteRepo r) = F.pretty r

instance Repo Repository where
    repoKey (LocalRepo (LocalRepository path _ _)) = Local path -- fromJust . parseURI $ "file://" ++ envPath path
    repoKey (RemoteRepo (RemoteRepository uri _)) = Remote uri
    repoReleaseInfo (LocalRepo (LocalRepository _ _ info)) = info
    repoReleaseInfo (RemoteRepo (RemoteRepository _ info)) = info

data LocalRepository
    = LocalRepository
      { repoRoot_ :: EnvPath
      , repoLayout_ :: (Maybe Layout)
      , repoReleaseInfoLocal_ :: [Release]
      } deriving (Read, Show)

instance Ord LocalRepository where
    compare (LocalRepository a _ _) (LocalRepository b _ _) = compare a b

instance Eq LocalRepository where
    a == b = compare a b == EQ

data RemoteRepository
    = RemoteRepository URI' [Release]
    deriving (Read, Show)

instance Ord RemoteRepository where
    compare (RemoteRepository a _) (RemoteRepository b _) = compare a b

instance Eq RemoteRepository where
    a == b = compare a b == EQ

instance F.Pretty RemoteRepository where
    pretty (RemoteRepository s _) = F.pretty s

repoRoot :: LocalRepository -> EnvPath
repoRoot = repoRoot_

repoLayout :: LocalRepository -> Maybe Layout
repoLayout = repoLayout_

repoReleaseInfoLocal :: LocalRepository -> [Release]
repoReleaseInfoLocal = repoReleaseInfoLocal_

-- |The possible file arrangements for a repository.  An empty
-- repository does not yet have either of these attributes.
data Layout = Flat | Pool deriving (Eq, Ord, Read, Show)

instance Repo LocalRepository where
    repoKey (LocalRepository path _ _) = Local path -- fromJust . parseURI $ "file://" ++ envPath path
    repoReleaseInfo (LocalRepository _ _ info) = info

class MonadIO m => MonadRepoCache m where
    getRepoCache :: m (Map RepoKey Repository)
    putRepoCache :: Map RepoKey Repository -> m ()

modifyRepoCache :: MonadRepoCache m => (Map RepoKey Repository -> Map RepoKey Repository) -> m ()
modifyRepoCache f = do
    s <- getRepoCache
    putRepoCache (f s)

fromLocalRepository :: LocalRepository -> Repository
fromLocalRepository = LocalRepo

-- | Load the value of the repo cache map from a file as a substitute for
-- downloading information from the remote repositories.  These values may
-- go out of date, as when a new release is added to a repository.  When this
-- happens some ugly errors will occur and the cache will have to be flushed.
loadRepoCache :: (MonadRepoCache m, MonadTop m) => m ()
loadRepoCache =
    do dir <- sub "repoCache"
       liftIO (loadRepoCache' dir `catch` (\ (e :: SomeException) -> qPutStrLn (show e) >> return Map.empty)) >>= putRepoCache
    where
      loadRepoCache' :: FilePath -> IO (Map RepoKey Repository)
      loadRepoCache' repoCache =
          do qPutStrLn "Loading repo cache..."
             file <- readFile repoCache
             case maybeRead file of
               Nothing ->
                   error ("Ignoring invalid repoCache: " ++ show file)
               Just pairs ->
                   qPutStrLn ("Loaded " ++ show (length pairs) ++ " entries from the repo cache.") >>
                   return (fromList pairs)

-- | Write the repo cache map into a file.
saveRepoCache :: (MonadIO m, MonadTop m, MonadRepoCache m) => m ()
saveRepoCache =
          do path <- sub "repoCache"
             live <- getRepoCache
             repoCache <- liftIO $ loadCache path
             let merged = show . Map.toList $ Map.union live repoCache
             liftIO (F.removeLink path `IO.catch` (\e -> unless (isDoesNotExistError e) (ioError e))) >> liftIO (writeFile path merged)
             return ()
          where
            -- isRemote uri = uriScheme uri /= "file:"
            -- isRemote (uri, _) = uriScheme uri /= "file:"
            loadCache :: FilePath -> IO (Map.Map RepoKey Repository)
            loadCache path =
                readFile path `IO.catch` (\ (_ :: SomeException) -> return "[]") >>=
                return . Map.fromList . fromMaybe [] . maybeRead

readLocalRepo :: MonadRepoCache m => EnvPath -> Maybe Layout -> m LocalRepository
readLocalRepo root layout =
    do state <- getRepoCache
       case Map.lookup (Local root) state of
         Just (RemoteRepo (RemoteRepository _ _)) -> error "readLocalRepo: internal error" -- somehow a remote repo got associated with a Local RepoKey
         Just (LocalRepo repo) -> return repo
         Nothing ->
             do names <- liftIO (getDirectoryContents distDir) >>= return . filter (\ x -> not . elem x $ [".", ".."])
                (links, dists) <- partitionM (liftIO . isSymLink . (distDir </>)) names
                linkText <- mapM (liftIO . F.readSymbolicLink) (map (distDir </>) links)
                let aliasPairs = zip linkText links ++ map (\ dist -> (dist, dist)) dists
                let distGroups = groupBy fstEq . sort $ aliasPairs
                let aliases = map (checkAliases  . partition (uncurry (==))) distGroups
                releaseInfo <- mapM (liftIO . getReleaseInfo) aliases
                qPutStrLn ("LocalRepository releaseInfo " ++ show root ++ ": " ++ show releaseInfo)
                let repo = LocalRepository { repoRoot_ = root
                                           , repoLayout_ = layout
                                           , repoReleaseInfoLocal_ = releaseInfo }
                putRepoCache (insertWith (\ _ x -> x) (Local root) (LocalRepo repo) state)
                return repo
    where
      fstEq (a, _) (b, _) = a == b
      checkAliases :: ([(String, String)], [(String, String)]) -> (ReleaseName, [ReleaseName])
      checkAliases ([(realName, _)], aliases) = (parseReleaseName realName, map (parseReleaseName . snd) aliases)
      checkAliases _ = error "Symbolic link points to itself!"
      getReleaseInfo :: (ReleaseName, [ReleaseName]) -> IO Release
      getReleaseInfo (dist, aliases) = parseReleaseFile (releasePath dist) dist aliases
      releasePath dist = distDir </> releaseName' dist ++ "/Release"
      distDir = outsidePath root ++ "/dists"

isSymLink :: FilePath -> IO Bool
isSymLink path = F.getSymbolicLinkStatus path >>= return . F.isSymbolicLink

parseReleaseFile :: FilePath -> ReleaseName -> [ReleaseName] -> IO Release
parseReleaseFile path dist aliases =
    liftIO (F.readFile path) >>= return . parseRelease dist aliases

parseRelease :: ReleaseName -> [ReleaseName] -> F.File Text -> Release
parseRelease name aliases file =
    case F.text file of
      Failure msgs -> error $ "Could not read " ++ show (F.path file) ++ ": " ++ show msgs
      Success text ->
          case T.parseControl (show (F.path file)) text of
            Left msg -> error $ "Failure parsing " ++ show (F.path file) ++ ": " ++ show msg
            Right (T.Control []) -> error $ "Empty release file: " ++ show (F.path file)
            Right (T.Control (info : _)) -> makeReleaseInfo (F.File {F.path = F.path file, F.text = Success info}) name aliases

-- | Create or verify the existance of the directories which will hold
-- a repository on the local machine.  Verify the index files for each of
-- its existing releases.
prepareLocalRepository :: MonadRepoCache m => EnvPath -> Maybe Layout -> m LocalRepository
prepareLocalRepository root layout =
    do mapM_ (liftIO . initDir)
                 [(".", 0o40755),
                  ("dists", 0o40755),
                  ("incoming", 0o41755),
                  ("removed", 0o40750),
                  ("reject", 0o40750)]
       layout' <- liftIO (computeLayout (outsidePath root)) >>= return . maybe layout Just
                  -- >>= return . maybe (maybe (error "No layout specified for new repository") id layout) id
       mapM_ (liftIO . initDir)
                 (case layout' of
                    Just Pool -> [("pool", 0o40755), ("installed", 0o40755)]
                    Just Flat -> []
                    Nothing -> [])
       readLocalRepo root layout'
    where
      initDir (name, mode) =
          do let path = outsidePath root </> name
             filterM (\ f -> doesDirectoryExist f >>= return . not) [path] >>=
                     mapM_ (\ f -> createDirectoryIfMissing True f)
             actualMode <- F.getFileStatus path >>= return . F.fileMode
             when (mode /= actualMode) (F.setFileMode path mode)
{-      notSymbolicLink root name =
          getSymbolicLinkStatus (root ++ "/dists/" ++ name) >>= return . not . isSymbolicLink
      hasReleaseFile root name =
          doesFileExist (root ++ "/dists/" ++ name ++ "/Release") -}

-- |Change the root directory of a repository.  FIXME: This should
-- also sync the repository to ensure consistency.
-- repoCD :: EnvPath -> LocalRepository -> LocalRepository
-- repoCD path repo = repo { repoRoot_ = path }

copyLocalRepo :: MonadRepoCache m => EnvPath -> LocalRepository -> m LocalRepository
copyLocalRepo dest repo =
    do qPutStrLn ("Syncing local repository from " ++ src ++ " -> " ++ dst)
       liftIO $ createDirectoryIfMissing True (outsidePath dest)
       result <- liftIO $ rsync [] (outsidePath (repoRoot repo)) (outsidePath dest)
       case result of
         ExitSuccess ->
             do let repo' = repo {repoRoot_ = dest}
                modifyRepoCache (Map.insert (Local dest) (LocalRepo repo'))
                return repo'
         code -> error $ "*** FAILURE syncing local repository " ++ src ++ " -> " ++ dst ++ ": " ++ show code
    where
      src = outsidePath (repoRoot repo)
      dst = outsidePath dest

-- |Try to determine a repository's layout.
computeLayout :: FilePath -> IO (Maybe Layout)
computeLayout root =
    do
      -- If there are already .dsc files in the root directory
      -- the repository layout is Flat.
      isFlat <- getDirectoryContents root >>= return . (/= []) . catMaybes . map (matchRegex (mkRegex "\\.dsc$"))
      -- If the pool directory already exists the repository layout is
      -- Pool.
      isPool <- doesDirectoryExist (root ++ "/pool")
      case (isFlat, isPool) of
        (True, _) -> return (Just Flat)
        (False, True) -> return (Just Pool)
        _ -> return Nothing

-- | Create or update the compatibility level file for a repository.
setRepositoryCompatibility :: LocalRepository -> IO ()
setRepositoryCompatibility r =
    maybeWriteFile path text
    where text = show libraryCompatibilityLevel ++ "\n"
          path = outsidePath (repoRoot r) </> compatibilityFile

-- | Return the subdirectory where a source package with the given
-- section and name would be installed given the layout of the
-- repository.
poolDir :: LocalRepository -> Section -> String -> FilePath
poolDir r section source =
    case repoLayout r of
      Just Pool ->
          "pool/" ++ sectionName' section </> prefixDir </> source
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

-- | Remove all the packages from the repository and then re-create
-- the empty releases.
flushLocalRepository :: MonadRepoCache m => LocalRepository -> m LocalRepository
flushLocalRepository r =
    do liftIO $ removeRecursiveSafely (outsidePath (repoRoot r))
       prepareLocalRepository (repoRoot r) (repoLayout r)

prepareRepository :: MonadRepoCache m => RepoKey -> m Repository
prepareRepository key =
    case key of
      Local path -> prepareLocalRepository path Nothing >>= return . LocalRepo
      Remote uri -> prepareRepository' (fromURI' uri)

-- | Prepare a repository, which may be remote or local depending on
-- the URI.
prepareRepository' :: MonadRepoCache m => URI -> m Repository
prepareRepository' uri =
    do state <- getRepoCache
       repo <- maybe newRepo return (Map.lookup (Remote (toURI' uri)) state)
       putRepoCache (Map.insert (Remote (toURI' uri)) repo state)
       return repo
    where
      newRepo =
             case uriScheme uri of
               "file:" -> prepareLocalRepository (EnvPath (EnvRoot "") (uriPath uri)) Nothing >>= return . LocalRepo
               -- FIXME: We only want to verifyRepository on demand.
               _ -> verifyRepository (toURI' uri)

-- |To create a RemoteRepo we must query it to find out the
-- names, sections, and supported architectures of its releases.
{-# NOINLINE verifyRepository #-}
verifyRepository :: MonadRepoCache m => URI' -> m Repository
verifyRepository uri =
    do state <- getRepoCache
       case Map.lookup (Remote uri) state of
         Just repo -> return repo
         Nothing ->
             do releaseInfo <- liftIO . unsafeInterleaveIO . getReleaseInfoRemote . fromURI' $ uri
                let repo = RemoteRepo (RemoteRepository uri releaseInfo)
                modifyRepoCache (Map.insert (Remote uri) repo)
                return repo

-- Nice code to do caching, but I figured out how to fix the old code.

--instance Read URI where
--    readsPrec _ s = [(fromJust (parseURI s), "")]
--
---- |Get the list of releases of a remote repository.
--getReleaseInfo :: FilePath
--               -> Bool     -- ^ If False don't look at existing cache
--               -> URI
--               -> IO [ReleaseInfo]
--getReleaseInfo top tryCache uri =
--    readCache >>= \ cache ->
--    return (lookup uri cache) >>=
--    maybe (updateCache cache) return
--    where
--      cachePath = top ++ "/repoCache"
--      readCache :: IO [(URI, [ReleaseInfo])]
--      readCache = if tryCache
--                  then try (readFile cachePath >>= return . read) >>= return . either (\ (_ :: SomeException) -> []) id
--                  else return []
--      updateCache :: [(URI, [ReleaseInfo])] -> IO [ReleaseInfo]
--      updateCache pairs = getReleaseInfoRemote uri >>= \ info ->
--                          writeCache ((uri, info) : pairs) >> return info
--      writeCache :: [(URI, [ReleaseInfo])] -> IO ()
--      writeCache pairs = writeFile (show pairs) cachePath

-- |Get the list of releases of a remote repository.
getReleaseInfoRemote :: URI -> IO [Release]
getReleaseInfoRemote uri =
    qPutStr ("(verifying " ++ uriToString' uri ++ ".") >>
    quieter 2 (dirFromURI distsURI) >>=
    quieter 2 . either (error . show) verify >>=
    return . catMaybes >>= 
    (\ result -> qPutStr ")\n" >> return result)
    where
      distsURI = uri {uriPath = uriPath uri </> "dists/"}
      verify names =
          do let dists = map parseReleaseName names
             (releaseFiles :: [F.File (T.Paragraph' Text)]) <- mapM getReleaseFile dists
             let releasePairs = zip3 (map getSuite releaseFiles) releaseFiles dists
             return $ map (uncurry3 getReleaseInfo) releasePairs
      releaseNameField releaseFile = case fmap T.unpack (T.fieldValue "Origin" releaseFile) of Just "Debian" -> "Codename"; _ -> "Suite"
      getReleaseInfo :: Maybe Text -> (F.File T.Paragraph) -> ReleaseName -> Maybe Release
      getReleaseInfo Nothing _ _ = Nothing
      getReleaseInfo (Just dist) _ relname | (parseReleaseName (T.unpack dist)) /= relname = Nothing
      getReleaseInfo (Just dist) info _ = Just $ makeReleaseInfo info (parseReleaseName (T.unpack dist)) []
      getSuite :: F.File (T.Paragraph' Text) -> Maybe Text
      getSuite (F.File {F.text = Success releaseFile}) = T.fieldValue (releaseNameField releaseFile) releaseFile
      getSuite (F.File {F.text = Failure msgs}) = fail (intercalate "\n" msgs)
      getReleaseFile :: ReleaseName -> IO (F.File (T.Paragraph' Text))
      getReleaseFile distName =
          do qPutStr "."
             release <- fileFromURI releaseURI
             let control = either Left (either (Left . toException . ErrorCall . show) Right . T.parseControl (show releaseURI) . Deb.decode) release
             case control of
               Right (T.Control [info :: T.Paragraph' Text]) -> return $ F.File {F.path = F.RemotePath releaseURI, F.text = Success info}
               _ -> error ("Failed to get release info from dist " ++ show (relName distName) ++ ", uri " ++ show releaseURI)
          where
            releaseURI = distURI {uriPath = uriPath distURI </> "Release"}
            distURI = distsURI {uriPath = uriPath distsURI </> releaseName' distName}
      uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
      uncurry3 f (a, b, c) =  f a b c

repoReleaseNames :: Repository -> [ReleaseName]
repoReleaseNames (RemoteRepo (RemoteRepository _ rels)) = map releaseName rels
repoReleaseNames (LocalRepo (LocalRepository _ _ rels)) = map releaseName rels
