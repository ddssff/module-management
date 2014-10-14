{-# LANGUAGE RecordWildCards #-}
-- |This module contains functions governing the assignment of version
-- numbers, patterned after Ubuntu version number policy:
--
-- (1) To distinguish packages which have been pulled out of one vendor's
-- repository to build for another, we add a vendor name and a build
-- number, so version "1.2-3" becomes "1.2-3vendorname1".  Subsequent
-- builds (perhaps due to changes in build dependencies) would be numbered
-- "1.2-3vendorname2" and so on.
--
-- (2) In addition, packages which are backported, (i.e. built for a
-- non-development release such as Debian etch or Ubuntu feisty) need
-- another tag to distinguish them from the version that would go into
-- the development release (sid or, as of this writing, heron.)  So if
-- we pulled the source for version "1.2-3vendorname4" out of our pool
-- to build for feisty, it would become "1.2-3vendorname4~feisty1".
--
-- (3) The first two policies combine if we
-- are building a package pulled directly from the other vendor into
-- our feisty pool, then "1.2-3" would become 1.2-3vendorname0~feisty1.
-- Subsequent builds of "1.2-3" (perhaps due to changes in build
-- dependencies) would get increasing build numbers,
-- "1.2-3vendorname0~feisty2" etc.
--
-- (4) If the original version number does not end with a digit, a "0" is
-- inserted before the vendor name to facilitate parsing of these tags.
--
-- (5) Finally, an additional tag format is supported for the benefit of
-- one autobuilder client, where before the vendor name an "r" and an
-- integer are inserted.
module Debian.VersionPolicy
    ( VersionTag
    , parseTag
    , getTag
    , dropTag
    , setTag
    , appendTag
    , tagCmp
    , tagMax
    , bumpTag
    , newTag
    , compareSourceAndDist
    ) where

import Control.Applicative.Error (Failing(..))
import Debian.Version ( buildDebianVersion, epoch, evr, revision, version, DebianVersion, prettyDebianVersion )
import Text.Regex ( matchRegex, mkRegex )
import Data.List ( sortBy )
import Data.Maybe ( catMaybes, fromMaybe, isJust, isNothing, listToMaybe )

-- |We implement two types of version number tags.  One has the format
--   @r<releasenumber>vendor<buildnumber>~release<buildnumber>@
-- the other simply
--   @vendor<buildnumber>~release<buildnumber>@
-- There are notes from a meeting that explains why ReleaseTagBuild is
-- more future friendly, but it is also more ugly.
data VersionTag
    = VersionTag { extraNumber :: Maybe Int		-- The number following the "r" (do not
							-- use in new applications.)
                 , vendorTag :: (String, Int)		-- The vendor name and build number
                 , releaseTag :: Maybe (String, Int)	-- The release name and build number
                 } deriving (Show, Eq)

-- | Parse a Debian revision string (the portion of the version number
-- following the final dash) into a prefix and a VersionTag.
parseTag :: [String] -> DebianVersion -> (DebianVersion, Maybe VersionTag)
parseTag vendors ver =
    fromMaybe (ver, Nothing) . listToMaybe . filter (isJust . snd) . map (`tryTag` ver) $ vendors
    where
      tryTag vendor ver' =
        let (e, v, r) = evr ver' in
        let (prefix, tag) =
                case r of
                  Nothing -> (Nothing, Nothing)
                  Just s ->
                      case matchRegex re s of
                        Nothing -> (Just s, Nothing)
                        Just [prefix1, prefix2, buildNo, "", _, _, _, _] ->
                            (Just (prefix1 ++ prefix2),
                             Just (VersionTag { extraNumber = Nothing
                                              , vendorTag = (vendor, read buildNo)
                                              , releaseTag = Nothing }))
                        Just [prefix1, prefix2, buildNo, _, releaseName, _, _, releaseNo] ->
                            (Just (prefix1 ++ prefix2),
                             Just (VersionTag { extraNumber = Nothing
                                              , vendorTag = (vendor, read buildNo)
                                              , releaseTag = Just (releaseName, read releaseNo) }))
                        Just result -> error $ "Internal error: " ++ show result in
        -- Try to parse the r5 from the end of the prefix.
        let (prefix', tag') =
                case (maybe Nothing (matchRegex extraRE) prefix, tag) of
                  (Just [prefix1, prefix2, digits], Just tag'') -> 
                      (Just (prefix1 ++ prefix2), Just (tag'' {extraNumber = Just (read digits)}))
                  _ -> (prefix, tag) in
        (buildDebianVersion e v (if prefix' == Just "0" then Nothing else prefix'), tag')
        where
          re = mkRegex (prefixRE ++ digitsRE ++ vendorRE ++ "(" ++ releaseRE ++ ")?$")
          vendorRE = escapeForRegex vendor ++ "([0-9]+)"
          prefixRE = "^(.*[^0-9])?"
          digitsRE = "([0-9]+)"
          releaseRE = "~(([^0-9]+)|(bpo[0-9]+\\+))([0-9]+)"
          extraRE = mkRegex "^(.*)([0-9]+)r([0-9]+)"

-- Do I have to write this?
escapeForRegex :: String -> String
escapeForRegex s =
    concatMap escape s
    where escape '+' = "\\+"
          escape c = [c]

-- | The tag returned by splitTag
getTag :: [String] -> DebianVersion -> Maybe VersionTag
getTag vendors ver = snd (parseTag vendors ver)

-- | The prefix returned by splitTag
dropTag :: [String] -> DebianVersion -> DebianVersion
dropTag vendors ver = fst (parseTag vendors ver)

-- |Modify a version number by adding or changing the vendor tag.  The
-- result will be newer than the distVersion (the newest already
-- uploaded version.)  It will also be different from (though not
-- necessarily newer than) any of the elements of allVersions
setTag :: (String -> String)
       -- ^ The release name alias function.  As an example, this would map
       -- the repository etch to the tag bpo40+, as specified by Debian policy.
       -> String
       -- ^ Other release names we manage that might appear in the repository.
       -> [String]
       -- ^ The vendor tag
       -> Maybe String
       -- ^ The build release, or Nothing if this is a development release
       -> Maybe Int
       -- ^ Use old "r0vendor1" format
       -> Maybe DebianVersion
       -- ^ The newst version that is already present in the dist.  We need
       -- to generate a version number newer than this.
       -> [DebianVersion]
       -- ^ All the versions that are currently available in the pool.  The
       -- result must not be a member of this list.
       -> DebianVersion
       -- ^ The version number that appears in the newest change log
       -- entry of the source package.  The result will be this number
       -- with a version tag added.  If this version is older than the
       -- current version with its tag stripped, the package cannot be
       -- built.
       -> Failing DebianVersion
       -- ^ The modified version number
{-setTag alias vendor oldVendors release extra distVersion allVersions sourceVersion =
    error ("vendor=" ++ show vendor ++ ", release=" ++ show release ++ ", extra=" ++ show extra ++ ", distVersion=" ++ show distVersion ++ ", allVersions=" ++ show allVersions ++ ", sourceVersion=" ++ show sourceVersion)-}
setTag alias vendor oldVendors release extra distVersion allVersions sourceVersion =
    case oldTag of
      Failure msgs -> Failure msgs
      Success x -> Success . appendTag alias sourceUpstreamVersion . Just . findAvailableTag . newTag' $ x
    where
      oldTag =
            case maybe Nothing (Just . parseTag (vendor : oldVendors)) distVersion of
              Nothing -> Success Nothing
              Just (distUpstreamVersion, distTag) ->
                  case compare sourceUpstreamVersion distUpstreamVersion of
                    LT -> Failure ["Source version " ++ show (prettyDebianVersion sourceVersion) ++ " (upstream: " ++ show (prettyDebianVersion sourceUpstreamVersion) ++ ")" ++
                                   " is too old to trump uploaded version " ++ show (prettyDebianVersion distUpstreamVersion) ++ " (dist: " ++ maybe "Nothing" (show . prettyDebianVersion) distVersion ++ ")"]
                    GT -> Success Nothing
                    -- Make sure we have the new vendor tag, not the one that
                    -- we just parsed.
                    EQ -> Success (maybe Nothing (Just . fixVendor vendor) distTag)
      fixVendor vendor' tag = let (_, n) = vendorTag tag in tag {vendorTag = (vendor', n)}
      -- The new tag must
      --  1) be newer than the old tag
      --  2) be at least as new as the tag in the source code changelog.
      --  3) have the appropriate releaseTag
      -- The oldTag may not have an appropriate releaseTag, because the
      -- distribution may have recently switched from development to
      -- non-development.  In that case we will have to bump the
      -- vendor build number, not the release build number, and then
      -- add the release tag.
      newTag' Nothing =
          VersionTag {vendorTag = (vendor, 1), releaseTag = maybe Nothing (\ relName -> Just (relName, 1)) release, extraNumber = extra}
      newTag' (Just distTag) =
          let distTag' = fixReleaseName release (bumpTag (fixVendor vendor distTag)) in
          let sourceTag' = maybe Nothing (Just . setReleaseName release) sourceTag in
          case tagMax [Just distTag', sourceTag'] of
            Nothing -> error $ "Internal error"
            Just tag -> tag
      (sourceUpstreamVersion, sourceTag) = parseTag (vendor : oldVendors) sourceVersion
      -- All the tags of existing packages whose upstream version matches the source
      allTags = catMaybes (map snd (filter (\ (v, _) -> v == sourceUpstreamVersion) (map (parseTag (vendor : oldVendors)) allVersions)))
      -- Repeatedly increment candidate until it differs from the
      -- elements of all.  Note that this is not the same as taking
      -- the maximum element and incrementing it, the value we want
      -- only needs to be not less than candidate.
      findAvailableTag :: VersionTag -> VersionTag
      findAvailableTag candidate =
          if elem candidate allTags then findAvailableTag (bumpTag candidate) else candidate

tagCmp :: Maybe VersionTag -> Maybe VersionTag -> Ordering
tagCmp (Just tagA) (Just tagB) =
    let (_, a) = vendorTag tagA
        (_, b) = vendorTag tagB in
    case compare a b of
      EQ -> case (releaseTag tagA, releaseTag tagB) of
              (Just (_, c), Just (_, d)) -> compare c d
              (Nothing, Nothing) -> EQ
              (Nothing, _) -> LT
              (_, Nothing) -> GT
      x -> x
tagCmp Nothing Nothing = EQ
tagCmp Nothing _ = LT
tagCmp _ Nothing = GT

tagMax :: [Maybe VersionTag] -> Maybe VersionTag
tagMax tags = head (sortBy (flip tagCmp) tags)

bumpTag :: VersionTag -> VersionTag
bumpTag tag@(VersionTag {releaseTag = Just (relName, relBuild)}) = tag {releaseTag = Just (relName, relBuild + 1)}
bumpTag tag@(VersionTag {vendorTag = (name, build), releaseTag = Nothing}) = tag {vendorTag = (name, build + 1)}

-- If one of the version number candidates has the wrong release name
-- this function fixes it, ensuring that the new tag isn't trumped by
-- the old.
fixReleaseName :: Maybe String -> VersionTag -> VersionTag
fixReleaseName release tag@(VersionTag {vendorTag = (vendorName, vendorBuild)}) =
    case (release, releaseTag tag) of
      (Just relName, Just (oldRelName, _)) | relName == oldRelName -> tag
      -- If the release name doesn't match we need to bump the vendor build
      (Just relName, _) -> tag {vendorTag = (vendorName, vendorBuild+1), releaseTag = Just (relName, 1)}
      -- Removing an existing release tag always increases the version
      (Nothing, Just _) -> tag {releaseTag = Nothing}
      (Nothing, Nothing) -> tag {vendorTag = (vendorName, vendorBuild+1)}

-- This is similer to fixReleaseName, but we don't require the result to
-- trump the argument.  This is applied to the source package version number,
-- which is not necessarily present in the dist.
setReleaseName :: Maybe String -> VersionTag -> VersionTag
setReleaseName release tag =
    case (release, releaseTag tag) of
      (Just relName, Just (oldRelName, _)) | relName == oldRelName -> tag
      (Just relName, Just _) -> tag {releaseTag = Just (relName, 1)}
      (Just relName, Nothing) -> tag {releaseTag = Just (relName, 1)}
      (Nothing, _) -> tag {releaseTag = Nothing}

-- Create a tag which is one click newer.
newTag :: String -> Maybe String -> Maybe Int -> VersionTag
newTag vendor Nothing extra = VersionTag { extraNumber = extra, vendorTag = (vendor, 1), releaseTag = Nothing }
newTag vendor (Just name) extra = VersionTag { extraNumber = extra, vendorTag = (vendor, 0), releaseTag = Just (name, 1) }

-- | Format a tag as a string.
showTag :: (String -> String) -> VersionTag -> String
showTag alias (VersionTag {..}) =
   let (vendor, vendorBuildNumber) = vendorTag in
   maybe "" (("r" ++) . show) extraNumber ++
         vendor ++ show vendorBuildNumber ++
         maybe "" (\ (relname, relbuild) -> "~" ++ alias relname ++ show relbuild) releaseTag

-- | Append a vendor tag to a string containing the revision portion
-- of a debian version number.
appendTag :: (String -> String) -> DebianVersion -> Maybe VersionTag -> DebianVersion
appendTag _ ver Nothing = ver
appendTag alias ver (Just tag) =
    case revision ver of
      Nothing -> setRevision ver (Just ("0" ++ showTag alias tag))
      Just rev -> case matchRegex numericSuffixRE rev of
                    Just [_, ""] -> setRevision ver (Just (rev ++ "0" ++ showTag alias tag))
                    Just [_, _] -> setRevision ver (Just (rev ++ showTag alias tag))
                    _ -> error "internal error"
    where numericSuffixRE = mkRegex "^(.*[^0-9])?([0-9]*)$"

-- | Change the revision part of a Debian version number.  (This may
-- belong in Debian.Version.)
setRevision :: DebianVersion -> Maybe String -> DebianVersion
setRevision ver rev = buildDebianVersion (epoch ver) (version ver) rev

-- Compare the version seen in the source code changelog to a version
-- seen in the distribution.  Since the autobuilder adds tags to the
-- version number of the packages it builds, the distribution version
-- may have been built from the source version even if they differ.
-- Specifically, we must assume that if the version matches when we
-- strip off one or both sections of the tag on the distribution
-- version number.
compareSourceAndDist :: [String] -> DebianVersion -> DebianVersion -> Ordering
compareSourceAndDist vendor s d =
    let (verS, tagS) = parseTag vendor s
        (verD, tagD) = parseTag vendor d in
    let venS = maybe Nothing (Just . vendorTag) tagS
        venD = maybe Nothing (Just . vendorTag) tagD
        relS = maybe Nothing releaseTag tagS
        relD = maybe Nothing releaseTag tagD in
    case () of
      _ | verS /= verD -> compare verS verD
        | isNothing venS && isJust venD -> EQ
        | isNothing relS && isJust relD -> EQ
        | True -> compare s d
