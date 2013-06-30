{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.ReExported
    ( -- * Source and binary packages
      TH.FixityDirection(..)
    , TH.Fixity(..)
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import qualified Language.Haskell.TH.Syntax as TH (Fixity(..), FixityDirection(..))
