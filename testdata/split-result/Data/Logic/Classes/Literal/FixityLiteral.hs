{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal.FixityLiteral
    ( fixityLiteral
    ) where

import Data.Logic.Classes.Literal.Literal (Literal(foldLiteral))
import Data.Logic.Classes.Pretty (Fixity(..), FixityDirection(..), HasFixity(..))

-- |Literals are the building blocks of the clause and implicative normal
-- |forms.  They support negation and must include True and False elements.
fixityLiteral :: (Literal formula atom) => formula -> Fixity
fixityLiteral formula =
    foldLiteral neg tf at formula
    where
      neg _ = Fixity 5 InfixN
      tf _ = Fixity 10 InfixN
      at = fixity


