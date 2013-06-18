{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal.ZipLiterals
    ( zipLiterals
    ) where

import Data.Logic.Classes.Literal.Literal (Literal(foldLiteral))

-- |Literals are the building blocks of the clause and implicative normal
-- |forms.  They support negation and must include True and False elements.
zipLiterals :: Literal lit atom =>
               (lit -> lit -> Maybe r)
            -> (Bool -> Bool -> Maybe r)
            -> (atom -> atom -> Maybe r)
            -> lit -> lit -> Maybe r
zipLiterals neg tf at fm1 fm2 =
    foldLiteral neg' tf' at' fm1
    where
      neg' p1 = foldLiteral (neg p1) (\ _ -> Nothing) (\ _ -> Nothing) fm2
      tf' x1 = foldLiteral (\ _ -> Nothing) (tf x1) (\ _ -> Nothing) fm2
      at' a1 = foldLiteral (\ _ -> Nothing) (\ _ -> Nothing) (at a1) fm2

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

