{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal.FromLiteral
    ( fromLiteral
    ) where

import Data.Logic.Classes.Constants (Constants(fromBool))
import qualified Data.Logic.Classes.FirstOrder as FOF (FirstOrderFormula)
import Data.Logic.Classes.Formula (Formula(atomic))
import Data.Logic.Classes.Literal.Literal (Literal(foldLiteral))
import Data.Logic.Classes.Negate ((.~.))

fromLiteral :: forall lit atom v fof atom2. (Literal lit atom, FOF.FirstOrderFormula fof atom2 v) =>
               (atom -> atom2) -> lit -> fof
fromLiteral ca lit = foldLiteral (\ p -> (.~.) (fromLiteral ca p)) fromBool (atomic . ca) lit


