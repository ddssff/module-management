{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal.FoldAtomsLiteral
    ( foldAtomsLiteral
    ) where

import Data.Logic.Classes.Literal.Literal (Literal(foldLiteral))

-- |Literals are the building blocks of the clause and implicative normal
-- |forms.  They support negation and must include True and False elements.
foldAtomsLiteral :: Literal lit atom => (r -> atom -> r) -> r -> lit -> r
foldAtomsLiteral f i lit = foldLiteral (foldAtomsLiteral f i) (const i) (f i) lit

