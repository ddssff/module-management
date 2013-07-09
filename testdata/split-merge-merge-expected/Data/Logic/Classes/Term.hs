{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}
module Data.Logic.Classes.Term
    ( Term(..)
    , Function
    , convertTerm
    , showTerm
    , prettyTerm
    , fvt
    , tsubst
    , funcs
    ) where

import Data.Generics (Data)
import Data.List (intercalate, intersperse)
import Data.Logic.Classes.Pretty (Pretty)
import Data.Logic.Classes.Skolem (Skolem)
import Data.Logic.Classes.Variable (Variable)
import qualified Data.Map as Map (lookup, Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (empty, Set, singleton, union, unions)
import Text.PrettyPrint ((<>), brackets, Doc, hcat, text)

class (Eq f, Ord f, Skolem f v, Data f, Pretty f) => Function f v

class ( Ord term  -- For implementing Ord in Literal
      , Variable v
      , Function f v ) => Term term v f | term -> v f where
    vt :: v -> term
    -- ^ Build a term which is a variable reference.
    fApp :: f -> [term] -> term
    -- ^ Build a term by applying terms to an atomic function.  @f@
    -- (atomic function) is one of the type parameters, this package
    -- is mostly indifferent to its internal structure.
    foldTerm :: (v -> r) -> (f -> [term] -> r) -> term -> r
    -- ^ A fold for the term data type, which understands terms built
    -- from a variable and a term built from the application of a
    -- primitive function to other terms.
    zipTerms :: (v -> v -> Maybe r) -> (f -> [term] -> f -> [term] -> Maybe r) -> term -> term -> Maybe r

convertTerm :: forall term1 v1 f1 term2 v2 f2.
               (Term term1 v1 f1,
                Term term2 v2 f2) =>
               (v1 -> v2) -> (f1 -> f2) -> term1 -> term2
convertTerm convertV convertF term =
    foldTerm v fn term
    where
      convertTerm' = convertTerm convertV convertF
      v = vt . convertV
      fn x ts = fApp (convertF x) (map convertTerm' ts)

showTerm :: forall term v f. (Term term v f, Show v, Show f) =>
            term -> String
showTerm term =
    foldTerm v f term
    where
      v :: v -> String
      v v' = "vt (" ++ show v' ++ ")"
      f :: f -> [term] -> String
      f fn ts = "fApp (" ++ show fn ++ ") [" ++ intercalate "," (map showTerm ts) ++ "]"

prettyTerm :: forall v f term. (Term term v f) =>
              (v -> Doc)
           -> (f -> Doc)
           -> term
           -> Doc
prettyTerm pv pf t = foldTerm pv (\ fn ts -> pf fn <> brackets (hcat (intersperse (text ",") (map (prettyTerm pv pf) ts)))) t

fvt :: (Term term v f, Ord v) => term -> Set.Set v
fvt tm = foldTerm Set.singleton (\ _ args -> Set.unions (map fvt args)) tm

-- ------------------------------------------------------------------------- 
-- Substitution within terms.                                                
-- ------------------------------------------------------------------------- 

tsubst :: (Term term v f, Ord v) => Map.Map v term -> term -> term
tsubst sfn tm = foldTerm (\ x -> fromMaybe tm (Map.lookup x sfn)) (\ fn args -> fApp fn (map (tsubst sfn) args)) tm

funcs :: (Term term v f, Ord f) => term -> Set.Set (f, Int)
funcs tm =
    foldTerm (const Set.empty)
             (\ f args -> foldr (\ arg r -> Set.union (funcs arg) r) (Set.singleton (f, length args)) args)
             tm
