{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal.Literal
    ( Literal(foldLiteral)
    ) where


import Data.Logic.Classes.Constants (Constants)
import Data.Logic.Classes.Formula (Formula)
import Data.Logic.Classes.Negate (Negatable)
import Data.Logic.Classes.Pretty (HasFixity)

-- |Literals are the building blocks of the clause and implicative normal
-- |forms.  They support negation and must include True and False elements.
-- |Literals are the building blocks of the clause and implicative normal
-- |forms.  They support negation and must include True and False elements.
class (Negatable lit, Constants lit, HasFixity atom, Formula lit atom, Ord lit) => Literal lit atom | lit -> atom where
    foldLiteral :: (lit -> r) -> (Bool -> r) -> (atom -> r) -> lit -> r


