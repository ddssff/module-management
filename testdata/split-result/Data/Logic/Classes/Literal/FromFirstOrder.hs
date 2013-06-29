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

{- This makes bad things happen.
-- | We can use an fof type as a lit, but it must not use some constructs.
instance FirstOrderFormula fof atom v => Literal fof atom v where
    foldLiteral neg tf at fm = foldFirstOrder qu co tf at fm
        where qu = error "instance Literal FirstOrderFormula"
              co ((:~:) x) = neg x
              co _ = error "instance Literal FirstOrderFormula"
    atomic = Data.Logic.Classes.FirstOrder.atomic
-}

-- |Just like Logic.FirstOrder.convertFOF except it rejects anything
-- with a construct unsupported in a normal logic formula,
-- i.e. quantifiers and formula combinators other than negation.
fromFirstOrder :: forall formula atom v lit atom2.
                  (Formula lit atom2, FOF.FirstOrderFormula formula atom v, Literal lit atom2) =>
                  (atom -> atom2) -> formula -> Failing lit
fromFirstOrder ca formula =
    FOF.foldFirstOrder (\ _ _ _ -> Failure ["fromFirstOrder"]) co (Success . fromBool) (Success . atomic . ca) formula
    where
      co :: Combination formula -> Failing lit
      co ((:~:) f) =  fromFirstOrder ca f >>= return . (.~.)
      co _ = Failure ["fromFirstOrder"]

