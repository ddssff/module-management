{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.ReExported
    ( TH.FixityDirection(..)
    , TH.Fixity(..)
    ) where

import qualified Language.Haskell.TH.Syntax as TH (Fixity(..), FixityDirection(..))
