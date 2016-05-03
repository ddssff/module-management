-- | This dependency solver determines which binary packages to install
-- in order to satisfy a set of dependency relations.  It uses a brute
-- force method, but tweaked to the point where it is usually able to
-- complete on real-world inputs.
--
-- Author: David Fox <dsf@seereason.com>
{-# LANGUAGE OverloadedStrings, PackageImports, ScopedTypeVariables, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fwarn-missing-signatures #-}
module Debian.Repo.Dependencies
    ( simplifyRelations
    , solutions
    , testArch
    , prettySimpleRelation
    , showSimpleRelation
    , readSimpleRelation
    ) where

import Control.Arrow (second)
import Data.List (groupBy, intercalate, nub, sortBy)
import qualified Data.Map as Map (empty, findWithDefault, fromListWith, insert, lookup, Map)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Set (toList)
import qualified Data.Set as Set (Set, singleton, toList, union)
import Debian.Arch (Arch(Source, Binary), ArchCPU(..), ArchOS(..))
import Debian.Control ()
import qualified Debian.Control.Text as S ()
import Debian.Pretty (ppPrint)
import Debian.Relation (ArchitectureReq(..), BinPkgName(BinPkgName), Relation(..), Relations, VersionReq(..))
import Debian.Repo.PackageID (PackageID(PackageID, packageName, packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage, packageID, pProvides)
import Debian.Repo.Prelude (cartesianProduct)
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import Text.PrettyPrint.HughesPJClass (Doc, text)

type Excuse = String

type ProvidesMap = Map.Map BinPkgName [BinaryPackage]

-- |A SimpleRelation just specifies a particular version of a package.
-- The Nothing relation is always satisified.
type SimpleRelation = Maybe (PackageID BinPkgName)

prettySimpleRelation :: SimpleRelation -> Doc
prettySimpleRelation Nothing = text "Nothing"
prettySimpleRelation (Just p) = ppPrint (packageName p) <> text "=" <> prettyDebianVersion (packageVersion p)

showSimpleRelation :: PackageID BinPkgName -> String
showSimpleRelation v = show (prettySimpleRelation (Just v))

-- Shouldn't this return a maybe?
readSimpleRelation :: String -> PackageID BinPkgName
readSimpleRelation s = case second (parseDebianVersion . (drop 1)) (span (/= '=') s) of
                         (n, v) -> PackageID { packageName = BinPkgName n, packageVersion = v }

-- |Each element is an or-list of specific package versions.
type SimpleRelations = [[SimpleRelation]]

-- Does this relation apply to this architecture?
testArch :: Arch -> Relation -> Bool
testArch _ (Rel _ _ Nothing) = True
testArch architecture (Rel _ _ (Just (ArchOnly archList))) = any (testArch' architecture) (toList archList)
testArch architecture (Rel _ _ (Just (ArchExcept archList))) = not (any (testArch' architecture) (toList archList))

testArch' :: Arch -> Arch -> Bool
testArch' (Binary os1 cpu1) (Binary os2 cpu2) = testOS os1 os2 && testCPU cpu1 cpu2
testArch' Source Source = True
testArch' _ _ = False

testOS :: ArchOS -> ArchOS -> Bool
testOS _ ArchOSAny = True
testOS (ArchOS a) (ArchOS b) = a == b
testOS ArchOSAny _ = error "testOS - invalid argument"

testCPU :: ArchCPU -> ArchCPU -> Bool
testCPU _ ArchCPUAny = True
testCPU (ArchCPU a) (ArchCPU b) = a == b
testCPU ArchCPUAny _ = error "testCPU - invalid argument"

-- |Turn the expressive inequality style relations to a set of simple
-- equality relations on only the packages in the available list.
simplifyRelations :: [BinaryPackage]
                  -> Relations
                  -> [BinPkgName]	-- ^ Given several alternative packages which satisfy
				        -- the relation, sort by name in this order.
                  -> Arch		-- ^ The build architecture
                  -> SimpleRelations
simplifyRelations available relations preferred arch =
    -- Sort the or-relations so that
    --  1. the packages named in the preferred list appear before other packages,
    --  2. the newest package appears first
    -- But we need to preserve the order that packages with different names appear
    map (sortBy prefOrder . sortBy versionOrder) relationsSimplified
    where
      relationsSimplified = expandVirtual arch nameMap providesMap relationsOfArch
          where
            nameMap = listMap (map (\ package -> (packageName (packageID package), package)) available)
            providesMap =
                listMap (concat (map (\ package -> 
                                      let names = packageName (packageID package) : map provides (pProvides package) in
                                      map (\ name -> (name, package)) names) available))
            provides [Rel name Nothing Nothing] = name
            provides bzzt = error ("Invalid relation in Provides: " ++ show (map ppPrint bzzt))
            relationsOfArch = filter (/= []) (map (nub . filter (testArch arch)) relations)
      prefOrder a b = compare (isPreferred a) (isPreferred b)
          where isPreferred = maybe False (flip elem preferred . packageName)
      versionOrder :: SimpleRelation -> SimpleRelation -> Ordering
      versionOrder (Just a) (Just b) | packageName a /= packageName b = EQ
      versionOrder (Just a) (Just b) = compare (packageVersion b) (packageVersion a)
      versionOrder _ _ = EQ

-- |Replace all relations by sets of equality relations on the exact
-- package versions which are actually available in the current
-- environment and satisfy the original relation.
expandVirtual :: Arch -> ProvidesMap -> ProvidesMap -> Relations -> SimpleRelations
expandVirtual Source _ _ _ = undefined
expandVirtual arch nameMap providesMap relations =
    map (nub . concat . map expand) relations
    where
      -- A relation with no version or architecture requirement
      -- can be satisfied by a provides or a real package.
      expand :: Relation -> [SimpleRelation]
      -- If the relation only applies to other architectures it can be ignored.
      expand (Rel _ _ (Just (ArchOnly archList))) | not (any (testArch' arch) (toList archList)) = [Nothing]
      expand (Rel _ _ (Just (ArchExcept archList))) | any (testArch' arch) (toList archList) = [Nothing]
      expand (Rel name Nothing Nothing) = map eqRel (Map.findWithDefault [] name providesMap)
      expand rel@(Rel name _ _) = map eqRel (filter (satisfies rel . packageID) (Map.findWithDefault [] name nameMap))
      eqRel :: BinaryPackage -> SimpleRelation
      eqRel package =
          Just (PackageID {packageName = packageName (packageID package), packageVersion = packageVersion p})
          where p = packageID package
      -- Does this package satisfy the relation?
      satisfies :: Relation -> PackageID BinPkgName -> Bool
      satisfies rel pkg = testRel (packageVersion pkg) rel

-- Does this version satisfy the relation?
testRel :: DebianVersion -> Relation -> Bool
testRel _ (Rel _ Nothing _) = True
testRel ver1 (Rel _ (Just (LTE ver2)) _) = not (compare ver1 ver2 == GT)
testRel ver1 (Rel _ (Just (GRE ver2)) _) = not (compare ver1 ver2 == LT)
testRel ver1 (Rel _ (Just (SLT ver2)) _) = compare ver1 ver2 == LT
testRel ver1 (Rel _ (Just (EEQ ver2)) _) = compare ver1 ver2 == EQ
testRel ver1 (Rel _ (Just (SGR ver2)) _) = compare ver1 ver2 == GT

-- |Given a root and a dependency list, return a list of possible
-- solutions to the dependency set.  Each solution is a list of
-- package versions which satisfy all the dependencies.  Note that if
-- a package is mentioned in two different clauses of the dependency
-- list, both clauses must be satisfied:
--
-- * a (>= 3.0), a (<< 4.0) | b (>= 2.0), c (>= 1.0) becomes
--
-- * a (>= 3.0), a (<< 4.0), c (>= 1.0) OR a (>= 3.0), b (>= 2.0), c (>= 1.0)
--
-- * [[a (>= 3.0)], [a (<< 4.0), b (>= 2.0)], [c (>= 1.0)]] becomes
--
-- * [[a (>= 3.0), a (<< 4.0), c (>= 1.0)], [a (>= 3.0), b (>= 2.0), c (>= 1.0)]]
--
-- So we can use each clause to eliminate packages which cannot
-- satisfy the dependency set.
solutions :: [BinaryPackage]	-- ^ The packages available to satisfy dependencies
          -> SimpleRelations	-- ^ The dependency relations to be satisfied
          -> Int		-- ^ Give up after this many solutions are computed
          -> (Either String [(Int, [BinaryPackage])])
				-- ^ On success return the set of packages to install,
				-- and the solution's sequence number.  Also returns
				-- the modified list of dependency relations, with all
				-- inequalities replaced by equalities on the particular
				-- versions of each package which are available.
solutions available relations limit =
    -- Do any of the dependencies require packages that simply don't
    -- exist?  If so we don't have to search for solutions, there
    -- aren't any.
    case any (== []) relations of
      True -> Left "Unsatisfiable dependencies"
      False ->
          -- Turn the And-of-Ors dependency list into Or-of-And-of-Ands.
          -- Each element of the result represents a an alternative set of
          -- constraints which a solution must satisfy.  Each element of
          -- the alternative is a list of relations on a single package,
          -- all of which must be satisfied.
          let alternatives = map (map nub . groupByName) (cartesianProduct relations) in
          --let versions = map makeVersion available in
          -- Find a set of packages that satisfies the dependencies
          case solutions' 1 alternatives available' of
            -- Add more information about the failure to the error message.
            Left message ->
                let results = map (testAlternative available') (take 20 alternatives) in
                let (errors :: [String]) = catMaybes (map (either Just (const Nothing)) results) in
                Left (message ++ "\n" ++ intercalate "\n" errors)
            Right x -> Right x
    where
      available' = availMap available
      solutions' :: Int -> [[[SimpleRelation]]] -> AvailMap (BinaryPackage) -> Either String [(Int, [BinaryPackage])]
      solutions' _ [] _ = Left "All candidate solutions failed"
      solutions' count (alternative : alternatives) available'' =
          if count > limit
          then Left ("No solutions found in first " ++ show limit ++ " candidates")
          else case testAlternative available'' alternative of
                 Left _excuse ->
                     solutions' (count + 1) alternatives available''
                 Right solution ->
                     Right ((count, solution) : either (const []) id (solutions' (count + 1) alternatives available''))

-- |The alternative argument is a possible solution to the dependency
-- problem.  Each element of alternative represents the relations on a
-- particular package.  So each one needs to be satisfied for the
-- alternative to be satisfied.
testAlternative :: AvailMap (BinaryPackage) -> [[SimpleRelation]] -> Either Excuse [BinaryPackage]
testAlternative available alternative =
    if all (not . null) solution then
        Right (map head solution) else
        Left ("Couldn't satisfy these conditions:\n  " ++
              intercalate "\n  " (map (show . map prettySimpleRelation) (mask (map null solution) alternative))
              {- ++ " using the available packages: " ++ show available -})
    where
      -- solution :: [a]
      solution = map (testPackage available) alternative
      mask bits elems = map fst (filter snd (zip elems bits))

-- |Return the list of versions of a package that satisfy all of the
-- relations.
testPackage :: AvailMap (BinaryPackage) -> [SimpleRelation] -> [BinaryPackage]
testPackage available rels =
    foldl satisfies [] rels
    where
      -- Which of these packages satisfy the relation?
      satisfies versions Nothing = versions
      satisfies versions (Just pkg) =
          versions ++ maybe [] (filter (\ x -> packageVersion (packageID x) == packageVersion pkg) . Set.toList) (Map.lookup (packageName pkg) available)

groupByName :: [SimpleRelation] -> [[SimpleRelation]]
groupByName relations =
    groupBy (\ a b -> compareNames a b == EQ) (sortBy compareNames relations)
    where compareNames a b = compare (maybe Nothing (Just . packageName) a) (maybe Nothing (Just . packageName) b)

-- Turn a list of (k, a) pairs into a map from k -> [a].
listMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
listMap pairs =
    foldl insertPair Map.empty pairs
    where insertPair m (k,a) = Map.insert k (a : (Map.findWithDefault [] k m)) m

type AvailMap a = Map.Map BinPkgName (Set.Set a)

availMap :: [BinaryPackage] -> AvailMap BinaryPackage
availMap xs = Map.fromListWith Set.union (map (\ x -> (packageName (packageID x), Set.singleton x)) xs)
