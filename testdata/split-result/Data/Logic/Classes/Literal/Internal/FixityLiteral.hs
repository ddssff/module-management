{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal.Internal.FixityLiteral
    ( fixityLiteral
    ) where

import Data.Logic.Classes.Literal.Literal (Literal(foldLiteral))
import Data.Logic.Classes.Pretty (Fixity(..), FixityDirection(..), HasFixity(..))

fixityLiteral :: (Literal formula atom) => formula -> Fixity
fixityLiteral formula =
    foldLiteral neg tf at formula
    where
      neg _ = Fixity 5 InfixN
      tf _ = Fixity 10 InfixN
      at = fixity

