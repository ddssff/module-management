{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package
    ( sourceFilePaths
    , binaryPackageSourceVersion
    , binarySourceVersion
    , sourcePackageBinaryNames
    , sourceBinaryNames
    , toSourcePackage
    , toBinaryPackage
    , binaryPackageSourceID
    , sourcePackageBinaryIDs
    , sourcePackagesOfIndex
    , sourcePackagesOfCachedIndex
    , binaryPackagesOfIndex
    , binaryPackagesOfCachedIndex
    , getPackages
    , putPackages
    , releaseSourcePackages
    , releaseBinaryPackages
    , TH.FixityDirection(..)
    , TH.Fixity(..)
    ) where

import Control.Exception as E (catch, ErrorCall(..), SomeException(..), try)
import "mtl" Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString.Lazy.Char8 as L (ByteString, fromChunks)
import Data.Either (partitionEithers)
import Data.List as List (intercalate, intersperse, map, partition)
import Data.Maybe (catMaybes)
import Data.Set as Set (fromList, map, Set, unions)
import qualified Data.Text as T (concat, pack, Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Debian.Apt.Index (Compression(..), controlFromIndex)
import Debian.Arch (Arch(..), prettyArch)
import Debian.Control (ControlFunctions(asString, stripWS), formatParagraph, Paragraph')
import qualified Debian.Control.Text as B (Control'(Control), ControlFunctions(lookupP), Field, Field'(Field), fieldValue, Paragraph)
import Debian.Relation (BinPkgName(..))
import qualified Debian.Relation.Text as B (ParseRelations(..), Relations)
import Debian.Release (releaseName', sectionName')
import Debian.Repo.Monads.Apt (insertBinaryPackages, insertSourcePackages, lookupBinaryPackages, lookupSourcePackages, MonadApt(getApt, putApt), readParagraphs)
import Debian.Repo.PackageIndex (binaryIndexList, packageIndexPath, sourceIndexList)
import Debian.Repo.Types.AptCache (AptCache(aptArch, rootDir))
import Debian.Repo.Types.EnvPath (EnvRoot(rootPath), outsidePath)
import Debian.Repo.Types.PackageIndex (BinaryPackage(..), BinaryPackageLocal, binaryPackageName, makeBinaryPackageID, makeSourcePackageID, PackageID(..), PackageIndex(..), PackageIndexLocal, SourceControl(..), SourceFileSpec(SourceFileSpec, sourceFileName), SourcePackage(..))
import Debian.Repo.Types.Release (Release(releaseName))
import Debian.Repo.Types.Repo (RepoKey, repoKeyURI)
import Debian.Repo.Types.Repository (LocalRepository, MonadRepoCache, repoRoot)
import Debian.URI (fileFromURIStrict)
import Debian.Version (DebianVersion, parseDebianVersion)
import qualified Debian.Version as V (buildDebianVersion, epoch, revision, version)
import Extra.Files (writeAndZipFileWithBackup)
import qualified Language.Haskell.TH.Syntax as TH (FixityDirection(..), Fixity(..))
import Network.URI (escapeURIString, URI(..), URIAuth(..), uriToString)
import System.FilePath ((</>), takeDirectory)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix (getFileStatus)
import Text.Regex (matchRegex, mkRegex, splitRegex)

class Constants p where
    asBool :: p -> Maybe Bool
    fromBool :: Bool -> p

useConstants :: Constants p => p -> Maybe Bool
useConstants x = asBool x

uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""

sourceFilePaths :: SourcePackage -> Set FilePath
sourceFilePaths package =
    Set.map ((sourceDirectory package) </>) . Set.map sourceFileName . Set.fromList . sourcePackageFiles $ package

-- | Return the name and version number of the source package that
-- generated this binary package.
binaryPackageSourceVersion :: BinaryPackage -> Maybe (String, DebianVersion)
binaryPackageSourceVersion package =
    let binaryName = binaryPackageName package
        binaryVersion = packageVersion . packageID $ package in
    binarySourceVersion' binaryName binaryVersion (packageInfo package)

-- |Return the name and version number of the source package that
-- generated this binary package.
-- see also: 'binaryPackageSourceVersion'
binarySourceVersion :: Paragraph' T.Text -> Maybe ((BinPkgName, DebianVersion), (String, DebianVersion))
binarySourceVersion paragraph =
    let mBinaryName = fmap (BinPkgName . T.unpack) $ B.fieldValue "Package" paragraph
        mBinaryVersion = fmap (parseDebianVersion . T.unpack) $ B.fieldValue "Version" paragraph
    in
      case (mBinaryName, mBinaryVersion) of
        (Just binaryName, Just binaryVersion) ->
            fmap ((,) (binaryName, binaryVersion)) $ binarySourceVersion' binaryName binaryVersion paragraph
        _ -> Nothing

binarySourceVersion' :: (ControlFunctions a) => BinPkgName -> DebianVersion -> Paragraph' a -> Maybe (String, DebianVersion)
binarySourceVersion' binaryName binaryVersion paragraph =
    case (B.fieldValue "Source" paragraph) of
      Just source' ->
          case matchRegex re (asString source') of
            Just [name, _, ""] -> Just (name, binaryVersion)
            Just [name, _, version] -> Just (name, copyEpoch binaryVersion (parseDebianVersion version))
            _ -> error "internal error"
      Nothing ->
          Just (asString (unBinPkgName binaryName), binaryVersion)
    where
      re = mkRegex "^[ ]*([^ (]*)[ ]*(\\([ ]*([^ )]*)\\))?[ ]*$"
      -- In the Packages file the version number in the Source: field has
      -- the epoch number stripped off.  I don't know why - I should search
      -- the Debian policy manual for this.  This puts it back on.
      copyEpoch src dst = V.buildDebianVersion (V.epoch src) (V.version dst) (V.revision dst)

sourcePackageBinaryNames :: SourcePackage -> [BinPkgName]
sourcePackageBinaryNames package =
    sourceBinaryNames (sourceParagraph package)

sourceBinaryNames :: B.Paragraph -> [BinPkgName]
sourceBinaryNames paragraph =
    case B.fieldValue "Binary" paragraph of
      Just names -> List.map BinPkgName (splitRegex (mkRegex "[ ,\t\n]+") (T.unpack names))
      _ -> error ("Source package info has no 'Binary' field:\n" ++ (T.unpack . formatParagraph $ paragraph))

toSourcePackage :: PackageIndex -> B.Paragraph -> SourcePackage
toSourcePackage index package =
    case (B.fieldValue "Directory" package,
          B.fieldValue "Files" package,
          B.fieldValue "Package" package,
          maybe Nothing (Just . parseDebianVersion . T.unpack) (B.fieldValue "Version" package)) of
      (Just directory, Just files, Just name, Just version) ->
          case (parseSourcesFileList files, parseSourceParagraph package) of
            (Right files', Right para) ->
                SourcePackage
                { sourcePackageID = makeSourcePackageID (T.unpack name) version
                , sourceParagraph = package
                , sourceControl = para
                , sourceDirectory = T.unpack directory
                , sourcePackageFiles = files' }
            (Left messages, _) -> error $ "Invalid file list: " ++ show messages
            (_, Left messages) -> error $ "Error in source paragraph\n package=" ++ show package ++ "\n  index=" ++ show index ++ "\n  messages:\n   " ++ intercalate "\n   " messages
      x -> error $ "Missing info in source package control information in " ++ show index ++ " -> " ++ show x ++ " :\n" ++ T.unpack (formatParagraph package)
    where
      -- Parse the list of files in a paragraph of a Sources index.
      parseSourcesFileList :: T.Text -> Either [String] [SourceFileSpec]
      parseSourcesFileList text =
          merge . catMaybes . List.map parseSourcesFiles . lines . T.unpack $ text
      parseSourcesFiles line =
          case words line of
            [md5sum, size, name] -> Just (Right (SourceFileSpec md5sum (read size) name))
            [] -> Nothing
            _ -> Just (Left ("Invalid line in Files list: '" ++ show line ++ "'"))
      merge x = case partition (either (const True) (const False)) x of
                  (a, []) -> Left . catMaybes . List.map (either Just (const Nothing )) $ a
                  (_, a) -> Right . catMaybes . List.map (either (const Nothing) Just) $ a

parseSourceParagraph :: B.Paragraph -> Either [String] SourceControl
parseSourceParagraph p =
    -- Look up the required fields
    case (B.fieldValue "Package" p,
          B.fieldValue "Maintainer" p) of
      (Just source', Just maintainer') ->
          -- The optional fields can be parsed as pure values
          Right (SourceControl
                  { source = source'
                  , maintainer = maintainer'
                  , uploaders = maybe [] (: []) $ B.fieldValue "Uploaders" p
                  , packageSection = fmap stripWS $ B.fieldValue "Section" p
                  , packagePriority = fmap stripWS $ B.fieldValue "Priority" p
                  , buildDepends = maybe [] (: []) $ B.fieldValue "Build-Depends" p
                  , buildDependsIndep = maybe [] (: []) $ B.fieldValue "Build-Depends-Indep" p
                  , buildConflicts = maybe [] (: []) $ B.fieldValue "Build-Conflicts" p
                  , buildConflictsIndep = maybe [] (: []) $ B.fieldValue "Build-Conflicts-Indep" p
                  , standardsVersion = fmap stripWS $ B.fieldValue "Standards-Version" p
                  , homepage = fmap stripWS $ B.fieldValue "Homepage" p })
      _x -> Left ["parseSourceParagraph - One or more required fields (Package, Maintainer, Standards-Version) missing: " ++ show p]

toBinaryPackage :: Release -> PackageIndex -> B.Paragraph -> BinaryPackage
toBinaryPackage release index p =
    case (B.fieldValue "Package" p, B.fieldValue "Version" p) of
      (Just name, Just version) ->
          BinaryPackage
          { packageID =
                makeBinaryPackageID (T.unpack name) (parseDebianVersion (T.unpack version))
          , packageInfo = p
          , pDepends = tryParseRel $ B.lookupP "Depends" p
          , pPreDepends = tryParseRel $ B.lookupP "Pre-Depends" p
          , pConflicts = tryParseRel $ B.lookupP "Conflicts" p
          , pReplaces =  tryParseRel $ B.lookupP "Replaces" p
          , pProvides =  tryParseRel $ B.lookupP "Provides" p
          }
      _ -> error ("Invalid data in source index:\n " ++ packageIndexPath release index)

tryParseRel :: Maybe B.Field -> B.Relations
tryParseRel (Just (B.Field (_, relStr))) = either (error . show) id (B.parseRelations relStr)
tryParseRel _ = []

-- | Parse the /Source/ field of a binary package's control
-- information, this may specify a version number for the source
-- package if it differs from the version number of the binary
-- package.
binaryPackageSourceID :: PackageIndex -> BinaryPackage -> PackageID BinPkgName
binaryPackageSourceID (PackageIndex component _) package =
    case maybe Nothing (matchRegex re . T.unpack) (B.fieldValue "Source" (packageInfo package)) of
      Just [name, _, ""] -> makeBinaryPackageID name (packageVersion pid)
      Just [name, _, version] -> makeBinaryPackageID name (parseDebianVersion version)
      _ -> error "Missing Source attribute in binary package info"
    where
      -- sourceIndex = PackageIndex component Source
      pid = packageID package
      re = mkRegex "^[ ]*([^ (]*)[ ]*(\\([ ]*([^ )]*)\\))?[ ]*$"

sourcePackageBinaryIDs :: Arch -> PackageIndex -> SourcePackage -> [PackageID BinPkgName]
sourcePackageBinaryIDs Source _ _ = error "invalid argument"
sourcePackageBinaryIDs arch sourceIndex package =
    case (B.fieldValue "Version" info, B.fieldValue "Binary" info) of
      (Just version, Just names) -> List.map (binaryID (parseDebianVersion (T.unpack version))) $ splitRegex (mkRegex "[ ,]+") (T.unpack names)
      _ -> error ("Source package info has no 'Binary' field:\n" ++ (T.unpack . formatParagraph $ info))
    where
      -- Note that this version number may be wrong - we need to
      -- look at the Source field of the binary package info.
      binaryID version name = makeBinaryPackageID name version
      -- binaryIndex = sourceIndex { packageIndexArch = arch }
      info = sourceParagraph package

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

-- FIXME: assuming the index is part of the cache
sourcePackagesOfCachedIndex :: (AptCache a, MonadApt m) => a -> RepoKey -> Release -> PackageIndex -> m [SourcePackage]
sourcePackagesOfCachedIndex cache repo release index =
    do state <- getApt
       let cached = lookupSourcePackages path state
       status <- liftIO $ getFileStatus path `E.catch` (\ (_ :: IOError) -> error $ "Sources.list seems out of sync.  If a new release has been created you probably need to remove " ++ takeDirectory (rootPath (rootDir cache)) ++ " and try again - sorry about that.")
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = List.map (toSourcePackage index) paragraphs
                 putApt (insertSourcePackages path (status, packages) state)
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache repo release index

indexCacheFile :: (AptCache a) => a -> RepoKey -> Release -> PackageIndex -> FilePath
indexCacheFile apt repo release index =
    case (aptArch apt, packageIndexArch index) of
      (Binary _ _, Source) -> indexPrefix repo release index ++ "_source_Sources"
      (Binary _ _, arch@(Binary _ _)) -> indexPrefix repo release index ++ "_binary-" ++ show (prettyArch arch) ++ "_Packages"
      (x, _) -> error "Invalid build architecture: " ++ show x

indexPrefix :: RepoKey -> Release -> PackageIndex -> FilePath
indexPrefix repo release index =
    (escapeURIString (/= '@') ("/var/lib/apt/lists/" ++ uriText +?+ "dists_") ++
     releaseName' distro ++ "_" ++ (sectionName' $ section))
    where
      section = packageIndexComponent index
      uri = repoKeyURI repo
      distro = releaseName $ release
      scheme = uriScheme uri
      auth = uriAuthority uri
      path = uriPath uri
      userpass = maybe "" uriUserInfo auth
      reg = maybeOfString $ maybe "" uriRegName auth
      port = maybe "" uriPort auth
      (user, pass) = break (== ':') userpass
      user' = maybeOfString user
      pass' = maybeOfString pass
      uriText = prefix scheme user' pass' reg port path
      -- If user is given and password is not, the user name is
      -- added to the file name.  Otherwise it is not.  Really.
      prefix "http:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "http:" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix "ftp:" _ _ (Just host) _ path =
          host ++ escape path
      prefix "file:" Nothing Nothing Nothing "" path =
          escape path
      prefix "ssh:" (Just user) Nothing (Just host) port path =
          user ++ host ++ port ++ escape path
      prefix "ssh" _ _ (Just host) port path =
          host ++ port ++ escape path
      prefix _ _ _ _ _ _ = error ("invalid repo URI: " ++ (uriToString' . repoKeyURI $ repo))
      maybeOfString "" = Nothing
      maybeOfString s = Just s
      escape s = intercalate "_" (wordsBy (== '/') s)
      wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
      wordsBy p s =
          case (break p s) of
            (s, []) -> [s]
            (h, t) -> h : wordsBy p (drop 1 t)

(+?+) :: String -> String -> String
(+?+) a ('_' : b) = a +?+ b
(+?+) "" b = b
(+?+) a b =
    case last a of
      '_' -> (init a) +?+ b
      _ -> a ++ "_" ++ b

-- FIXME: assuming the index is part of the cache
binaryPackagesOfCachedIndex :: (MonadApt m, AptCache a) => a -> RepoKey -> Release -> PackageIndex -> m [BinaryPackage]
binaryPackagesOfCachedIndex cache repo release index =
    do state <- getApt
       let cached = lookupBinaryPackages path state
       status <- liftIO $ getFileStatus path
       case cached of
         Just (status', packages) | status == status' -> return packages
         _ -> do paragraphs <- liftIO $ unsafeInterleaveIO (readParagraphs path)
                 let packages = List.map (toBinaryPackage release index) paragraphs
                 putApt (insertBinaryPackages path (status, packages) state)
                 return packages
    where
      path = rootPath (rootDir cache) ++ indexCacheFile cache repo release index

-- | Return a list of all source packages.
releaseSourcePackages :: MonadRepoCache m => RepoKey -> Release -> m (Set SourcePackage)
releaseSourcePackages repo release =
    mapM (sourcePackagesOfIndex repo release) (sourceIndexList release) >>= return . test
    where
      test :: [Either SomeException [SourcePackage]] -> Set SourcePackage
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Return a list of all the binary packages for all supported architectures.
releaseBinaryPackages :: MonadRepoCache m => RepoKey -> Release -> m (Set BinaryPackage)
releaseBinaryPackages repo release =
    mapM (binaryPackagesOfIndex repo release) (binaryIndexList release) >>= return . test
    where
      test xs = case partitionEithers xs of
                  ([], ok) -> Set.unions (List.map Set.fromList ok)
                  (bad, _) -> error $ intercalate ", " (List.map show bad)

-- | Write a set of packages into a package index.
putPackages :: LocalRepository -> Release -> PackageIndexLocal ->  [BinaryPackageLocal] -> IO ()
putPackages repo release index packages =
    writeAndZipFileWithBackup (outsidePath (repoRoot repo) </> packageIndexPath release index) (L.fromChunks [encodeUtf8 text]) >>= either (fail . intercalate "\n") return
    where
      text = T.concat (intersperse (T.pack "\n") . List.map formatParagraph . List.map packageInfo $ packages)
