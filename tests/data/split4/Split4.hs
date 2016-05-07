{-# LANGUAGE ScopedTypeVariables #-}
module Split4 where

import Control.Exception
import Control.Monad.Trans
import Data.ByteString.Lazy as L hiding (intercalate, partition)
import Data.List as List
import Data.Maybe (catMaybes)
import Debian.Arch
import Debian.Repo
import Debian.Repo.State.PackageIndex
import Debian.Control (formatParagraph)
import Debian.Control.Text as B (Control'(Control), ControlFunctions(lookupP), Field, Field'(Field), fieldValue, Paragraph, stripWS)
import Debian.Relation as B(Relations, parseRelations)
import Debian.Version
import Debian.URI
import Debian.Apt.Index
import System.FilePath
import Data.Text as T (Text, unpack)

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
binaryPackagesOfIndex :: MonadIO m => RepoKey -> Release -> PackageIndex -> m (Either SomeException [BinaryPackage])
binaryPackagesOfIndex repo release index =
    case packageIndexArch index of
      Source -> return (Right [])
      _ -> liftIO $ getPackages repo release index -- >>= return . either Left (Right . List.map (toBinaryPackage index . packageInfo))

-- | Get the contents of a package index
sourcePackagesOfIndex :: MonadIO m => RepoKey -> Release -> PackageIndex -> m (Either SomeException [SourcePackage])
sourcePackagesOfIndex repo release index =
    case packageIndexArch index of
      Source -> liftIO (getPackages repo release index) >>= return . either Left (Right . List.map (toSourcePackage index . packageInfo))
      _ -> return (Right [])

toSourcePackage :: PackageIndex -> B.Paragraph -> SourcePackage
toSourcePackage index package =
    case (B.fieldValue "Directory" package,
          B.fieldValue "Files" package,
          B.fieldValue "Package" package,
          maybe Nothing (Just . parseDebianVersion' . T.unpack) (B.fieldValue "Version" package)) of
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
            [md5sum, size', name] -> Just (Right (SourceFileSpec md5sum (read size') name))
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
          { packageID = makeBinaryPackageID (T.unpack name) (parseDebianVersion' (T.unpack version))
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
