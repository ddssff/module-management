{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module CLI.Cabal.Instances () where

import Data.List
import Data.Map
import Data.Typeable
import Data.Version
import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName
import Distribution.Package
import Distribution.Package
import Distribution.PackageDescription
import Distribution.System
import Distribution.Version
import GHC.Generics
import Language.Haskell.Extension

deriving instance Generic a => Generic (Condition a)
#if MIN_VERSION_Cabal(1,21,1)
instance (Ord k, Generic k, Generic v) => Generic (Map k v) where
    type Rep (Map k v) = Rep [(k, v)]
    from = from . toList
    to = fromList . to
#else
deriving instance Generic Arch
deriving instance Generic Benchmark
deriving instance Generic BenchmarkInterface
deriving instance Generic BenchmarkType
deriving instance Generic BuildInfo
deriving instance Generic BuildType
deriving instance Generic CompilerFlavor
deriving instance Generic Dependency
deriving instance Generic Executable
deriving instance Generic Extension
deriving instance Generic FlagName
deriving instance Generic KnownExtension
deriving instance Generic Language
deriving instance Generic Library
deriving instance Generic License
deriving instance Generic OS
deriving instance Generic PackageDescription
deriving instance Generic PackageIdentifier
deriving instance Generic PackageName
deriving instance Generic RepoKind
deriving instance Generic RepoType
deriving instance Generic SourceRepo
deriving instance Generic TestSuite
deriving instance Generic TestSuiteInterface
deriving instance Generic TestType
deriving instance Generic VersionRange
#endif
deriving instance Generic ConfVar
deriving instance Generic Flag
deriving instance Generic GenericPackageDescription
deriving instance Generic Version

deriving instance (Generic a, Generic b, Generic c) => Generic (CondTree a b c)

#if MIN_VERSION_Cabal(1,18,0)
-- cabal 1.18 has these instances. There should be
-- a way to derive instance-if-doesn't-exist-already
#else
deriving instance Typeable3 CondTree
deriving instance Typeable1 Condition

#if !MIN_VERSION_Cabal(1,21,1)
deriving instance Typeable Arch
deriving instance Typeable Benchmark
deriving instance Typeable BenchmarkInterface
deriving instance Typeable BenchmarkType
deriving instance Typeable BuildInfo
deriving instance Typeable BuildType
deriving instance Typeable CompilerFlavor
deriving instance Typeable ConfVar
deriving instance Typeable Dependency
deriving instance Typeable Executable
deriving instance Typeable Extension
deriving instance Typeable Flag
deriving instance Typeable FlagName
deriving instance Typeable GenericPackageDescription
deriving instance Typeable KnownExtension
deriving instance Typeable Language
deriving instance Typeable Library
deriving instance Typeable License
deriving instance Typeable OS
deriving instance Typeable PackageDescription
deriving instance Typeable PackageIdentifier
deriving instance Typeable PackageName
deriving instance Typeable RepoKind
deriving instance Typeable RepoType
deriving instance Typeable SourceRepo
deriving instance Typeable TestSuite
deriving instance Typeable TestSuiteInterface
deriving instance Typeable TestType
deriving instance Typeable VersionRange
#endif

deriving instance Typeable ModuleName
#endif

#if !MIN_VERSION_Cabal(1,21,1)
instance Generic ModuleName where
    type Rep ModuleName = Rep [String]
    from = from . components
    to = fromString . intercalate "." . to
#endif
