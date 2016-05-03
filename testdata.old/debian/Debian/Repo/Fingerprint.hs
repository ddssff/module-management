{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Debian.Repo.Fingerprint
    ( RetrieveMethod(..)
    , RetrieveAttribute(..)
    , GitSpec(..)
    , Fingerprint(..)
    , retrieveMethodMD5
    , readUpstreamFingerprint
    , DownstreamFingerprint(..)
    , packageFingerprint
    , showFingerprint
    , dependencyChanges
    , showDependencies
    , showDependencies'
    ) where

import Control.Applicative.Error (maybeRead)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Data (Data)
import Data.Digest.Pure.MD5 (md5)
import Data.List as List (intercalate, map)
import Data.Set as Set (Set, toList, toAscList, difference, empty, fromList, map, filter)
import Data.Text (unpack, strip)
import Data.Typeable (Typeable)
import qualified Debian.Control.String as S
import Debian.Pretty (ppDisplay)
import Debian.Relation (BinPkgName(..))
import Debian.Repo.Dependencies (readSimpleRelation, showSimpleRelation)
import Debian.Repo.PackageID (PackageID(packageName, packageVersion))
import Debian.Repo.PackageIndex (SourcePackage(sourceParagraph, sourcePackageID))
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import Extra.Misc(columns)

-- | The methods we know for obtaining source code.
data RetrieveMethod
    = Apt String String                      -- ^ Apt dist name - download using apt-get (FIXME: Apt String SrcPkgName would be better, but that breaks read/show)
    | Bzr String                             -- ^ Download from a Bazaar repository
    | Cd FilePath RetrieveMethod             -- ^ Get the source code from a subdirectory of another download
    | Darcs String                           -- ^ Download from a Darcs repository
    | DataFiles RetrieveMethod RetrieveMethod FilePath
                                             -- ^ The first tree is a cabal package, copy the files in the second tree into
                                             -- the first at the location specified by FilePath.  Typically you would then patch
                                             -- the cabal file to add entries to the Data-Files list.
    | DebDir RetrieveMethod RetrieveMethod   -- ^ Combine the upstream download with a download for a debian directory
    | Debianize'' RetrieveMethod (Maybe String) -- ^ Retrieve a cabal package from Hackage and use cabal-debian to
                                              -- debianize it.  The optional string is used as the debian source
                                              -- package name if provided, this allows us to build several versions of
                                              -- the same source package - e.g. one with ghc, one with ghcjs.
                                              -- Remember we can't build more than one package with a given source
                                              -- package name, so this is the only attribute that needs to be part of
                                              -- the RetrieveMethod for a Debianization.  If not supplied the name is
                                              -- derived from the cabal package name.
    | Debianize RetrieveMethod               -- ^ Old Debianize constructor retained for backwards compatibility.
                                             -- we read this value from the old source packages.
    | Debianize' RetrieveMethod [()]         -- ^ Another old Debianize constructor
    | Dir FilePath                           -- ^ Retrieve the source code from a directory on a local machine
    | Git String [GitSpec]                   -- ^ Download from a Git repository, optional commit hashes and/or branch names
    | Hackage String                         -- ^ Download a cabal package from hackage
    | Hg String                              -- ^ Download from a Mercurial repository
    | Patch RetrieveMethod ByteString        -- ^ Apply the patch given in the string text to the target
    | Proc RetrieveMethod                    -- ^ Mount proc during the build (this should be a PackageFlag.)
    | Quilt RetrieveMethod RetrieveMethod    -- ^ Combine a download with a download of a patch directory to be applied by quilt
    | SourceDeb RetrieveMethod               -- ^ Download and unpack a source deb - a .dsc, a .tar.gz, and a .diff.gz file.
    | Svn String                             -- ^ Download from a Subversion repository
    | Tla String                             -- ^ Download from a TLA repository
    | Twice RetrieveMethod                   -- ^ Perform the build twice (should be a package flag)
    | Uri String String                      -- ^ Download a tarball from the URI.  The checksum is used to implement caching.
    | Zero                                   -- ^ Nothing, do not build
    deriving (Read, Show, Data, Typeable)

data GitSpec
    = Branch String
    | Commit String
    deriving (Read, Show, Eq, Data, Typeable)

-- | Information gathered as a result of the retrieveal.  If these
-- identifying characteristic of the source tree change between one
-- retrieve and the next, a build should be triggered.  Examples
-- include the latest Git commit identifier, or a darcs repository
-- tag.  A darcs commit string is not a suitable identifier because
-- darcs commits are not totally ordered, so it can't reliably be used
-- to reconstruct the specific source tree.  Instead we checksum the
-- entire darcs change history.
data RetrieveAttribute
    = AptVersion String
    -- ^ The version number of a package retrieved by apt-get source
    | GitCommit String
    -- ^ The id of the most recent commit
    | DarcsChangesId String
    -- ^ The checksum of the output of darcs changes --xml-output
    | SourceDebName String
    -- ^ The name of the Debian source package
    deriving (Read, Show, Eq, Ord, Data, Typeable)

retrieveMethodMD5 :: RetrieveMethod -> String
retrieveMethodMD5 = show . md5 . L.pack . show

-- | This type represents a package's fingerprint, (formerly its
-- revision string,) which includes three pieces of information: how
-- it was retrieved, the version number of the resulting Debian source
-- package, and the names and version numbers of the build
-- dependencies against which it was or is about to be built.
data Fingerprint
    = Fingerprint
        { method :: RetrieveMethod
          -- ^ The method which was used to retrieve the source code.
        , retrievedAttributes :: Set RetrieveAttribute
          -- ^ Identifying information about the result of the retrieve,
          -- e.g. the debian version number, darcs repository tag, git
          -- commit id.  Replaces upstreamVersion.
        , upstreamVersion :: DebianVersion
          -- ^ The version number in the changelog of the freshly downloaded
          -- package, before any suffix is added by the autobuilder.
        , buildDependencyVersions :: Set (PackageID BinPkgName)
          -- ^ The names and version numbers of the build dependencies which
          -- were present when the package was build.
        }
    deriving Show

data DownstreamFingerprint
    = DownstreamFingerprint
        { upstreamFingerprint :: Fingerprint
        , downstreamVersion :: DebianVersion
          -- ^ This will be the the version field plus the suffix that
          -- was added by the autobuilder.
        }
    deriving Show

packageFingerprint :: SourcePackage -> Maybe DownstreamFingerprint
packageFingerprint package =
    maybe Nothing readDownstreamFingerprint (fmap (unpack . strip) . S.fieldValue "Fingerprint" . sourceParagraph $ package)
    where
      readDownstreamFingerprint :: String -> Maybe DownstreamFingerprint
      readDownstreamFingerprint s =
          maybe Nothing
                (\ f -> Just $ DownstreamFingerprint { upstreamFingerprint = f
                                                     , downstreamVersion = packageVersion . sourcePackageID $ package })
                (readUpstreamFingerprint s)

-- | Read a Fingerprint from the string written by showFingerprint
-- into the .dsc file.
readUpstreamFingerprint :: String -> Maybe Fingerprint
readUpstreamFingerprint s =
    case readMethod s of
      Nothing -> Nothing
      Just (m, etc) ->
          -- See if there is a list of RetrieveAttribute - if not use the empty list
          let (attrs, etc') = case reads etc :: [([RetrieveAttribute], String)] of
                                [(x, etc'')] -> (x, etc'')
                                _ -> ([], etc) in
          case words etc' of
            (sourceVersion : buildDeps)
                | not (elem '=' sourceVersion) ->
                    Just $ Fingerprint { method = m
                                       , upstreamVersion = parseDebianVersion sourceVersion
                                       , retrievedAttributes = Set.fromList attrs
                                       , buildDependencyVersions = fromList (List.map readSimpleRelation buildDeps) }
            -- Old style fingerprint field - no upstream
            -- version number after the method.  I think
            -- at this point we can ignore these.
            _ -> Nothing

readMethod :: String -> Maybe (RetrieveMethod, String)
readMethod s =
    -- New style: read the method directly from the beginning of s
    case reads s :: [(RetrieveMethod, String)] of
      [(m, etc)] -> Just (fix m, etc)
      -- Old (broken) style: read a string, then read the method out
      -- of it
      _ -> case reads s :: [(String, String)] of
             [(m, etc)] -> case maybeRead m of
                             Nothing -> Nothing
                             Just m' -> Just (fix m', etc)
             _ -> Nothing
    where
      fix (Debianize x) = Debianize'' x Nothing
      fix (Debianize' x _) = Debianize'' x Nothing
      fix x = x

{-
modernizeMethod :: RetrieveMethod -> RetrieveMethod
modernizeMethod = everywhere (mkT modernizeMethod1)

modernizeMethod1 :: RetrieveMethod -> RetrieveMethod
modernizeMethod1 (Debianize p) = Debianize' p []
modernizeMethod1 x = x
-}

-- | The value that goes into the Fingerprint: field in the .dsc file.
showFingerprint :: Fingerprint -> String
showFingerprint (Fingerprint {method = m, upstreamVersion = sourceVersion, retrievedAttributes = attrs, buildDependencyVersions = versions}) =
    intercalate " " ["(" ++ show m ++ ")",
                     "[" ++ intercalate ", " (List.map show (toAscList attrs)) ++ "]",
                     show (prettyDebianVersion sourceVersion),
                     intercalate " " (List.map showSimpleRelation (toAscList versions))]

showDependencies :: Fingerprint -> [String]
showDependencies (Fingerprint {buildDependencyVersions = deps}) = toAscList $ Set.map showSimpleRelation deps

-- | Show the dependency list without the version numbers.
showDependencies' :: Fingerprint -> [String]
showDependencies' (Fingerprint {buildDependencyVersions = deps}) = toAscList $ Set.map (ppDisplay . packageName) deps

dependencyChanges :: Maybe DownstreamFingerprint -> Fingerprint -> String
dependencyChanges old new =
    depChanges changedDeps
    where
      depChanges [] = ""
      depChanges _ = "  * Build dependency changes:" ++ prefix ++ intercalate prefix padded ++ "\n"
      padded = List.map concat . columns . List.map showDepChange $ changedDeps
      changedDeps = Set.toList (Set.difference (buildDependencyVersions new) (buildDependencyVersions new))
      showDepChange newDep =
          case toList (Set.filter (hasName (packageName newDep)) (maybe empty (buildDependencyVersions . upstreamFingerprint) old)) of
            [] -> [" " ++ unBinPkgName (packageName newDep) ++ ": ", "(none)", " -> ", show (prettyDebianVersion (packageVersion newDep))]
            (oldDep : _) -> [" " ++ unBinPkgName (packageName newDep) ++ ": ", show (prettyDebianVersion (packageVersion oldDep)), " -> ", show (prettyDebianVersion (packageVersion newDep))]
      hasName name = ((== name) . packageName)
      prefix = "\n    "
