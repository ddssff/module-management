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
instance (Ord k, Generic k, Generic v) => Generic (Map k v) where
    type Rep (Map k v) = Rep [(k, v)]
    from = from . toList
    to = fromList . to
deriving instance Generic ConfVar
deriving instance Generic Flag
deriving instance Generic GenericPackageDescription
deriving instance Generic Version

deriving instance (Generic a, Generic b, Generic c) => Generic (CondTree a b c)
