{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, TypeFamilies #-}
-- | Substitution and finding variables are two basic operations on
-- formulas that contain terms and variables.  If a formula type
-- supports quantifiers we can also find free variables, otherwise all
-- variables are considered free.
module Data.Logic.Classes.Atom
    ( Atom(..)
    -- , Formula(..)
    ) where

import Control.Applicative.Error (Failing)
import qualified Data.Map as Map
import qualified Data.Set as Set

{-
class Formula formula term v | formula -> term v where
    substitute :: Map.Map v term -> formula -> formula
    allVariables :: formula -> Set.Set v
    freeVariables :: formula -> Set.Set v
    unify :: Map.Map v term -> formula -> formula -> Failing (Map.Map v term)
    match :: Map.Map v term -> formula -> formula -> Failing (Map.Map v term)
    -- ^ Very similar to unify, not quite sure if there is a difference
    foldTerms :: (term -> r -> r) -> r -> formula -> r
    isRename :: formula -> formula -> Bool
    getSubst :: Map.Map v term -> formula -> Map.Map v term
-}

class Atom atom term v | atom -> term v where
    substitute :: Map.Map v term -> atom -> atom
    allVariables :: atom -> Set.Set v
    freeVariables :: atom -> Set.Set v
    unify :: Map.Map v term -> atom -> atom -> Failing (Map.Map v term)
    match :: Map.Map v term -> atom -> atom -> Failing (Map.Map v term)
    -- ^ Very similar to unify, not quite sure if there is a difference
    foldTerms :: (term -> r -> r) -> r -> atom -> r
    isRename :: atom -> atom -> Bool
    getSubst :: Map.Map v term -> atom -> Map.Map v term

