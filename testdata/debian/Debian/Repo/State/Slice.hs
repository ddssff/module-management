{-# LANGUAGE FlexibleContexts, PackageImports, TemplateHaskell, TupleSections #-}
-- |Types that represent a "slice" of a repository, as defined by a
-- list of DebSource.  This is called a slice because some sections
-- may be omitted, and because different repositories may be combined
-- in the list.
module Debian.Repo.State.Slice
    ( verifySourcesList
    , repoSources
    , updateCacheSources
    ) where

import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B (concat)
import qualified Data.ByteString.Lazy.Char8 as L (toChunks)
import Data.List (nubBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Text as T (pack, Text, unpack)
import Debian.Control (Control'(Control), ControlFunctions(parseControl), fieldValue, Paragraph')
import Debian.Control.Text (decodeParagraph)
import Debian.Pretty (ppDisplay)
import Debian.Release (parseReleaseName, parseSection')
import Debian.Repo.EnvPath (EnvPath(..), EnvRoot(..), outsidePath)
import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.Prelude (replaceFile, symbol)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Repo (repoKey)
import Debian.Repo.Slice (NamedSliceList(sliceList, sliceListName), Slice(..), SliceList(..), SourcesChangedAction, doSourcesChangedAction)
import Debian.Repo.State.Repository (readLocalRepository, prepareRemoteRepository)
import Debian.Repo.Top (MonadTop, distDir, sourcesPath)
import Debian.Sources (DebSource(..), SourceType(Deb, DebSrc), parseSourcesList)
import Debian.URI (dirFromURI, fileFromURI)
import Network.URI (URI(uriScheme, uriPath))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Regex (mkRegex, splitRegex)

-- | Examine the repository whose root is at the given URI and return a
-- set of sources that includes all of its releases.  This is used to
-- ensure that a package we want to upload doesn't already exist in
-- the repository.
repoSources :: MonadRepos m => Maybe EnvRoot -> URI -> m SliceList
repoSources chroot uri =
    do dirs <- liftIO (uriSubdirs chroot (uri {uriPath = uriPath uri ++ "/dists/"}))
       releaseFiles <- mapM (liftIO . readRelease uri) dirs >>= return . catMaybes
       let codenames = map (maybe Nothing (zap (flip elem dirs))) . map (fieldValue "Codename") $ releaseFiles
           sections = map (maybe Nothing (Just . map parseSection' . splitRegex (mkRegex "[ \t,]+") . unpack) . fieldValue "Components") $ releaseFiles
           result = concat $ map sources . nubBy (\ (a, _) (b, _) -> a == b) . zip codenames $ sections
       mapM (verifyDebSource Nothing) result >>= (\ list -> return $ SliceList { slices = list })
    where
      sources (Just codename, Just components@(_ : _)) =
          [DebSource {sourceType = Deb, sourceUri = uri, sourceDist = Right (parseReleaseName (unpack codename), components)},
           DebSource {sourceType = DebSrc, sourceUri = uri, sourceDist = Right (parseReleaseName (unpack codename), components)}]
      sources _ = []
      -- Compute the list of sections for each dist on a remote server.
      zap p x = if p x then Just x else Nothing

-- |Return the list of releases in a repository, which is the
-- list of directories in the dists subdirectory.  Currently
-- this is only known to work with Apache.  Note that some of
-- the returned directories may be symlinks.
uriSubdirs :: (Maybe EnvRoot) -> URI -> IO [Text]
uriSubdirs root uri =
    liftIO (dirFromURI uri') >>= either throw (return . map pack)
    where
      uri' = case uriScheme uri of
               "file:" -> uri {uriPath = maybe "" rootPath root ++ (uriPath uri)}
               _ -> uri

readRelease :: URI -> Text -> IO (Maybe (Paragraph' Text))
readRelease uri name =
    do output <- liftIO (fileFromURI uri')
       case output of
         Left e -> throw e
         Right s -> case parseControl (show uri') (B.concat . L.toChunks $ s) of
                      Right (Control [paragraph]) -> return (Just (decodeParagraph paragraph))
                      _ -> return Nothing
    where
      uri' = uri {uriPath = uriPath uri </> "dists" </> unpack name </> "Release"}

-- | Make sure all the required local and remote repository objects
-- used by a sources.list file are in our cache.
verifySourcesList :: MonadRepos m => Maybe EnvRoot -> [DebSource] -> m SliceList
verifySourcesList chroot list =
    mapM (verifyDebSource chroot) list >>=
    (\ xs -> return $ SliceList { slices = xs })

verifyDebSource :: MonadRepos m => Maybe EnvRoot -> DebSource -> m Slice
verifyDebSource chroot line =
    case uriScheme (sourceUri line) of
      "file:" -> let path = EnvPath chroot' (uriPath (sourceUri line)) in readLocalRepository path Nothing >>= maybe (error $ "No repository at " ++ show (outsidePath path)) (\ repo' -> return $ Slice {sliceRepoKey = repoKey repo', sliceSource = line})
      _ -> prepareRemoteRepository (sourceUri line) >>= \ repo' -> return $ Slice {sliceRepoKey = repoKey repo', sliceSource = line}
    where
      chroot' = fromMaybe (EnvRoot "") chroot

-- |Change the sources.list of an AptCache object, subject to the
-- value of sourcesChangedAction.  (FIXME: Does this really work for MonadOS?)
updateCacheSources :: (MonadRepos m, MonadTop m) => SourcesChangedAction -> NamedSliceList -> m ()
updateCacheSources sourcesChangedAction baseSources = do
  let rel = sliceListName baseSources
  dir <- distDir rel
  sources <- sourcesPath rel
  distExists <- liftIO $ doesFileExist sources
  case distExists of
    True -> do
      fileSources <- liftIO (readFile sources) >>= verifySourcesList Nothing . parseSourcesList
      when (fileSources /= sliceList baseSources)
           (qPutStrLn ($(symbol 'updateCacheSources) ++ " for " <> show rel) >>
            liftIO (doSourcesChangedAction dir sources baseSources fileSources sourcesChangedAction))
    False -> do
      liftIO $ createDirectoryIfMissing True dir
      liftIO $ replaceFile sources (ppDisplay baseSources)
  return ()
