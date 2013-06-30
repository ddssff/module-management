{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal.ToPropositional
    ( toPropositional
    ) where

import Data.Logic.Classes.Constants (Constants(fromBool))
import Data.Logic.Classes.Formula (Formula(atomic))
import Data.Logic.Classes.Literal.Literal (Literal(foldLiteral))
import Data.Logic.Classes.Negate ((.~.))
import qualified Data.Logic.Classes.Propositional as P (PropositionalFormula)

toPropositional :: forall lit atom pf atom2. (Literal lit atom, P.PropositionalFormula pf atom2) =>
                   (atom -> atom2) -> lit -> pf
toPropositional ca lit = foldLiteral (\ p -> (.~.) (toPropositional ca p)) fromBool (atomic . ca) lit
