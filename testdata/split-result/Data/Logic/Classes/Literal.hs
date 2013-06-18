{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal
    ( Literal(..)
    , zipLiterals
    , fromFirstOrder
    , fromLiteral
    , toPropositional
    , prettyLit
    , foldAtomsLiteral
    ) where

import Data.Logic.Classes.Literal.FoldAtomsLiteral (foldAtomsLiteral)
import Data.Logic.Classes.Literal.FromFirstOrder (fromFirstOrder)
import Data.Logic.Classes.Literal.FromLiteral (fromLiteral)
import Data.Logic.Classes.Literal.Literal (Literal(foldLiteral))
import Data.Logic.Classes.Literal.PrettyLit (prettyLit)
import Data.Logic.Classes.Literal.ToPropositional (toPropositional)
import Data.Logic.Classes.Literal.ZipLiterals (zipLiterals)
