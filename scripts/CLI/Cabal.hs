{-# LANGUAGE FlexibleContexts, ViewPatterns #-}
-- | Adjust contents of a cabal file to follow
-- rearrangements of the underlying modules
module CLI.Cabal (

    module CLI.Cabal,

    readPackageDescription,
    writeGenericPackageDescription,
    showGenericPackageDescription,
    ) where

import CLI.Cabal.Instances

import Control.Lens
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import GHC.Generics (Generic)
import GHC.Generics.Lens

import Distribution.ModuleName


import Language.Haskell.Modules hiding (ModuleName)
import qualified Language.Haskell.Modules (ModuleName(ModuleName))
import Data.List
import Data.Maybe

import Language.Haskell.Modules.Common

import qualified Data.Set as S


toMN = Language.Haskell.Modules.ModuleName () . intercalate "." . components

fromMN (ModKey {_modName = Language.Haskell.Modules.ModuleName () x}) =  fromString x

getModules :: GenericPackageDescription -> [Language.Haskell.Modules.ModuleName ()]
getModules pkgDesc = map toMN $ toListOf tinplate pkgDesc
getSrcDirs pkgDesc = hsSourceDirs =<< toListOf tinplate pkgDesc

-- merge :: Generic a => [ModuleName] -> ModuleName -> a -> a
merge (map fromMN -> old) (fromMN -> new) pkgDesc = pkgDesc & over tinplate  f
    where f ms = let n | any (`elem` old) ms = [new::ModuleName]
                        | otherwise = []
                 in n ++ filter (`notElem` old) ms

split (fromMN -> old) (map fromMN -> news) pkgDesc = pkgDesc & over tinplate f
    where f ms = concatMap (\m -> if (m::ModuleName) == old then news else [m]) ms

update moduleResults pkgDesc = pkgDesc & over tinplate (\x -> fn x (S.fromList x))
    where
    r = removals moduleResults
    a = additions moduleResults
    fn modules ms
        | S.size (ms `S.intersection` r) > 0 = filter (`S.notMember` r) modules
                                            ++ S.toList (a S.\\ ms)
        | otherwise = modules :: [ModuleName]

removals rs = S.fromList $ mapMaybe remove rs
    where
    remove (ToBeRemoved k) = Just (fromMN k)
    remove (JustRemoved k) = Just (fromMN k)
    remove _ = Nothing

additions rs = S.fromList $ mapMaybe add rs
    where
    add (ToBeCreated k _) = Just (fromMN k)
    add (JustCreated k) = Just (fromMN k)
    add _ = Nothing


