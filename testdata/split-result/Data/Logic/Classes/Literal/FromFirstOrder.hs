{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal.FromFirstOrder
    ( fromFirstOrder
    ) where

import Data.Logic.Classes.Combine (Combination(..))
import Data.Logic.Classes.Constants (Constants(fromBool))
import qualified Data.Logic.Classes.FirstOrder as FOF (FirstOrderFormula(foldFirstOrder))
import Data.Logic.Classes.Formula (Formula(atomic))
import Data.Logic.Classes.Literal.Literal (Literal)
import Data.Logic.Classes.Negate ((.~.))
import Data.Logic.Failing (Failing(..))

-- |Literals are the building blocks of the clause and implicative normal
-- |forms.  They support negation and must include True and False elements.
fromFirstOrder :: forall formula atom v lit atom2.
                  (Formula lit atom2, FOF.FirstOrderFormula formula atom v, Literal lit atom2) =>
                  (atom -> atom2) -> formula -> Failing lit
fromFirstOrder ca formula =
    FOF.foldFirstOrder (\ _ _ _ -> Failure ["fromFirstOrder"]) co (Success . fromBool) (Success . atomic . ca) formula
    where
      co :: Combination formula -> Failing lit
      co ((:~:) f) =  fromFirstOrder ca f >>= return . (.~.)
      co _ = Failure ["fromFirstOrder"]


