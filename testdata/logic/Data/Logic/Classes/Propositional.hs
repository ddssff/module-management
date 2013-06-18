-- | PropositionalFormula is a multi-parameter type class for
-- representing instance of propositional (aka zeroth order) logic
-- datatypes.  These are formulas which have truth values, but no "for
-- all" or "there exists" quantifiers and thus no variables or terms
-- as we have in first order or predicate logic.  It is intended that
-- we will be able to write instances for various different
-- implementations to allow these systems to interoperate.  The
-- operator names were adopted from the Logic-TPTP package.
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Logic.Classes.Propositional
    ( PropositionalFormula(..)
    , showPropositional
    , prettyPropositional
    , fixityPropositional
    , convertProp
    , combine
    , negationNormalForm
    , clauseNormalForm
    , clauseNormalForm'
    , clauseNormalFormAlt
    , clauseNormalFormAlt'
    , disjunctiveNormalForm
    , disjunctiveNormalForm'
    , overatoms
    , foldAtomsPropositional
    , mapAtomsPropositional
    ) where

import Data.Logic.Classes.Combine
import Data.Logic.Classes.Constants (Constants(fromBool), asBool, prettyBool)
import Data.Logic.Classes.Formula (Formula(atomic))
import Data.Logic.Classes.Negate
import Data.Logic.Classes.Pretty (Pretty, HasFixity(fixity), Fixity(Fixity), FixityDirection(..))
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set.Extra as Set
import Text.PrettyPrint (Doc, text, (<>))

-- |A type class for propositional logic.  If the type we are writing
-- an instance for is a zero-order (aka propositional) logic type
-- there will generally by a type or a type parameter corresponding to
-- atom.  For first order or predicate logic types, it is generally
-- easiest to just use the formula type itself as the atom type, and
-- raise errors in the implementation if a non-atomic formula somehow
-- appears where an atomic formula is expected (i.e. as an argument to
-- atomic or to the third argument of foldPropositional.)
-- 
-- The Ord superclass is required so we can put formulas in sets
-- during the normal form computations.  Negatable and Combinable are
-- also considered basic operations that we can't build this package
-- without.  It is less obvious whether Constants is always required,
-- but the implementation of functions like simplify would be more
-- elaborate if we didn't have it, so we will require it.
class (Ord formula, Negatable formula, Combinable formula, Constants formula,
       Pretty formula, HasFixity formula, Formula formula atom) => PropositionalFormula formula atom | formula -> atom where
    -- | Build an atomic formula from the atom type.
    -- | A fold function that distributes different sorts of formula
    -- to its parameter functions, one to handle binary operators, one
    -- for negations, and one for atomic formulas.  See examples of its
    -- use to implement the polymorphic functions below.
    foldPropositional :: (Combination formula -> r)
                      -> (Bool -> r)
                      -> (atom -> r)
                      -> formula -> r

-- | Show a formula in a format that can be evaluated 
showPropositional :: (PropositionalFormula formula atom) => (atom -> String) -> formula -> String
showPropositional showAtom formula =
    foldPropositional co tf at formula
    where
      co ((:~:) f) = "(.~.) " ++ parenForm f
      co (BinOp f1 op f2) = parenForm f1 ++ " " ++ showFormOp op ++ " " ++ parenForm f2
      tf True = "true"
      tf False = "false"
      at = showAtom
      parenForm x = "(" ++ showPropositional showAtom x ++ ")"
      showFormOp (:<=>:) = ".<=>."
      showFormOp (:=>:) = ".=>."
      showFormOp (:&:) = ".&."
      showFormOp (:|:) = ".|."

-- | Show a formula in a visually pleasing format.
prettyPropositional :: (PropositionalFormula formula atom, HasFixity formula) =>
                       (atom -> Doc)
                    -> Fixity        -- ^ The fixity of the parent formula.  If the operator being formatted here
                                     -- has a lower precedence it needs to be parenthesized.
                    -> formula
                    -> Doc
prettyPropositional prettyAtom (Fixity pprec _pdir) formula =
    parenIf (pprec > prec) (foldPropositional co tf at formula)
    where
      co ((:~:) f) = text "¬" <> prettyPropositional prettyAtom fix f
      co (BinOp f1 op f2) = prettyPropositional prettyAtom fix f1 <> text " " <> prettyBinOp op <> text " " <> prettyPropositional prettyAtom fix f2
      tf = prettyBool
      at = prettyAtom
      -- parenForm x = cat [text "(", prettyPropositional prettyAtom 0 x, text ")"]
      parenIf True x = text "(" <> x <> text ")"
      parenIf False x = x
      fix@(Fixity prec _dir) = fixity formula

fixityPropositional :: (HasFixity atom, PropositionalFormula formula atom) => formula -> Fixity
fixityPropositional formula =
    foldPropositional co tf at formula
    where
      co ((:~:) _) = Fixity 5 InfixN
      co (BinOp _ (:&:) _) = Fixity 4 InfixL
      co (BinOp _ (:|:) _) = Fixity 3 InfixL
      co (BinOp _ (:=>:) _) = Fixity 2 InfixR
      co (BinOp _ (:<=>:) _) = Fixity 1 InfixL
      tf _ = Fixity 10 InfixN
      at = fixity

-- |Convert any instance of a propositional logic expression to any
-- other using the supplied atom conversion function.
convertProp :: forall formula1 atom1 formula2 atom2.
               (PropositionalFormula formula1 atom1,
                PropositionalFormula formula2 atom2) =>
               (atom1 -> atom2) -> formula1 -> formula2
convertProp convertA formula =
    foldPropositional c fromBool a formula
    where
      convert' = convertProp convertA
      c ((:~:) f) = (.~.) (convert' f)
      c (BinOp f1 op f2) = combine (BinOp (convert' f1) op (convert' f2))
      a = atomic . convertA

-- | Simplify and recursively apply nnf.
negationNormalForm :: (PropositionalFormula formula atom) => formula -> formula
negationNormalForm = nnf . psimplify

-- |Eliminate => and <=> and move negations inwards:
-- 
-- @
-- Formula      Rewrites to
--  P => Q      ~P | Q
--  P <=> Q     (P & Q) | (~P & ~Q)
-- ~∀X P        ∃X ~P
-- ~∃X P        ∀X ~P
-- ~(P & Q)     (~P | ~Q)
-- ~(P | Q)     (~P & ~Q)
-- ~~P  P
-- @
-- 
nnf :: (PropositionalFormula formula atom) => formula -> formula
nnf fm = foldPropositional (nnfCombine fm) fromBool (\ _ -> fm) fm

nnfCombine :: (PropositionalFormula formula atom) => formula -> Combination formula -> formula
nnfCombine fm ((:~:) p) = foldPropositional nnfNotCombine (fromBool . not) (\ _ -> fm) p
nnfCombine _ (BinOp p (:=>:) q) = nnf ((.~.) p) .|. (nnf q)
nnfCombine _ (BinOp p (:<=>:) q) =  (nnf p .&. nnf q) .|. (nnf ((.~.) p) .&. nnf ((.~.) q))
nnfCombine _ (BinOp p (:&:) q) = nnf p .&. nnf q
nnfCombine _ (BinOp p (:|:) q) = nnf p .|. nnf q

nnfNotCombine :: (PropositionalFormula formula atom) => Combination formula -> formula
nnfNotCombine ((:~:) p) = nnf p
nnfNotCombine (BinOp p (:&:) q) = nnf ((.~.) p) .|. nnf ((.~.) q)
nnfNotCombine (BinOp p (:|:) q) = nnf ((.~.) p) .&. nnf ((.~.) q)
nnfNotCombine (BinOp p (:=>:) q) = nnf p .&. nnf ((.~.) q)
nnfNotCombine (BinOp p (:<=>:) q) = (nnf p .&. nnf ((.~.) q)) .|. nnf ((.~.) p) .&. nnf q

-- |Do a bottom-up recursion to simplify a propositional formula.
psimplify :: (PropositionalFormula formula atom) => formula -> formula
psimplify fm =
    foldPropositional co tf at fm
    where
      co ((:~:) p) = psimplify1 ((.~.) (psimplify p))
      co (BinOp p (:&:) q) = psimplify1 (psimplify p .&. psimplify q)
      co (BinOp p (:|:) q) = psimplify1 (psimplify p .|. psimplify q)
      co (BinOp p (:=>:) q) = psimplify1 (psimplify p .=>. psimplify q)
      co (BinOp p (:<=>:) q) = psimplify1 (psimplify p .<=>. psimplify q)
      tf _ = fm
      at _ = fm

-- |Do one step of simplify for propositional formulas:
-- Perform the following transformations everywhere, plus any
-- commuted versions for &, |, and <=>.
-- 
-- @
--  ~False      -> True
--  ~True       -> False
--  True & P    -> P
--  False & P   -> False
--  True | P    -> True
--  False | P   -> P
--  True => P   -> P
--  False => P  -> True
--  P => True   -> P
--  P => False  -> True
--  True <=> P  -> P
--  False <=> P -> ~P
-- @
-- 
psimplify1 :: forall formula atom. (PropositionalFormula formula atom) => formula -> formula
psimplify1 fm =
    foldPropositional simplifyCombine (\ _ -> fm) (\ _ -> fm) fm
    where
      simplifyCombine ((:~:) f) = foldPropositional simplifyNotCombine (fromBool . not) simplifyNotAtom f
      simplifyCombine (BinOp l op r) =
          case (asBool l, op, asBool r) of
            (Just True,  (:&:), _)            -> r
            (Just False, (:&:), _)            -> fromBool False
            (_,          (:&:), Just True)    -> l
            (_,          (:&:), Just False)   -> fromBool False
            (Just True,  (:|:), _)            -> fromBool True
            (Just False, (:|:), _)            -> r
            (_,          (:|:), Just True)    -> fromBool True
            (_,          (:|:), Just False)   -> l
            (Just True,  (:=>:), _)           -> r
            (Just False, (:=>:), _)           -> fromBool True
            (_,          (:=>:), Just True)   -> fromBool True
            (_,          (:=>:), Just False)  -> (.~.) l
            (Just False, (:<=>:), Just False) -> fromBool True
            (Just True,  (:<=>:), _)          -> r
            (Just False, (:<=>:), _)          -> (.~.) r
            (_,          (:<=>:), Just True)  -> l
            (_,          (:<=>:), Just False) -> (.~.) l
            _                                 -> fm
      simplifyNotCombine ((:~:) f) = f
      simplifyNotCombine _ = fm
      simplifyNotAtom x = (.~.) (atomic x)

clauseNormalForm' :: (PropositionalFormula formula atom) => formula -> Set.Set (Set.Set formula)
clauseNormalForm' = simp purecnf . negationNormalForm

clauseNormalForm :: forall formula atom. (PropositionalFormula formula atom) => formula -> formula
clauseNormalForm formula =
    case clean (lists cnf) of
      [] -> fromBool True
      xss -> foldr1 (.&.) . map (foldr1 (.|.)) $ xss
    where
      clean = filter (not . null)
      lists = Set.toList . Set.map Set.toList
      cnf = clauseNormalForm' formula

-- |I'm not sure of the clauseNormalForm functions above are wrong or just different.
clauseNormalFormAlt' :: (PropositionalFormula formula atom) => formula -> Set.Set (Set.Set formula)
clauseNormalFormAlt' = simp purecnf' . negationNormalForm

clauseNormalFormAlt :: forall formula atom. (PropositionalFormula formula atom) => formula -> formula
clauseNormalFormAlt formula =
    case clean (lists cnf) of
      [] -> fromBool True
      xss -> foldr1 (.&.) . map (foldr1 (.|.)) $ xss
    where
      clean = filter (not . null)
      lists = Set.toList . Set.map Set.toList
      cnf = clauseNormalFormAlt' formula

disjunctiveNormalForm :: (PropositionalFormula formula atom) => formula -> formula
disjunctiveNormalForm formula =
    case clean (lists dnf) of
      [] -> fromBool False
      xss -> foldr1 (.|.) . map (foldr1 (.&.)) $ xss
    where
      clean = filter (not . null)
      lists = Set.toList . Set.map Set.toList
      dnf = disjunctiveNormalForm' formula

disjunctiveNormalForm' :: (PropositionalFormula formula atom, Eq formula) => formula -> Set.Set (Set.Set formula)
disjunctiveNormalForm' = simp purednf . negationNormalForm

simp :: forall formula atom. (PropositionalFormula formula atom) =>
        (formula -> Set.Set (Set.Set formula)) -> formula -> Set.Set (Set.Set formula)
simp purenf fm =
    case (compare fm (fromBool False), compare fm (fromBool True)) of
      (EQ, _) -> Set.empty
      (_, EQ) -> Set.singleton Set.empty
      _ ->cjs'
    where
      -- Discard any clause that is the proper subset of another clause
      cjs' = Set.filter keep cjs
      keep x = not (Set.or (Set.map (Set.isProperSubsetOf x) cjs))
      cjs = Set.filter (not . trivial) (purenf (nnf fm)) :: Set.Set (Set.Set formula)

-- |Harrison page 59.  Look for complementary pairs in a clause.
trivial :: (Negatable lit, Ord lit) => Set.Set lit -> Bool
trivial lits =
    not . Set.null $ Set.intersection (Set.map (.~.) n) p
    where (n, p) = Set.partition negated lits

purecnf :: forall formula atom. (PropositionalFormula formula atom) => formula -> Set.Set (Set.Set formula)
purecnf fm = Set.map (Set.map (.~.)) (purednf (nnf ((.~.) fm)))

purednf :: forall formula atom. (PropositionalFormula formula atom) => formula -> Set.Set (Set.Set formula)
purednf fm =
    foldPropositional c (\ _ -> x) (\ _ -> x)  fm
    where
      c :: Combination formula -> Set.Set (Set.Set formula)
      c (BinOp p (:&:) q) = Set.distrib (purednf p) (purednf q)
      c (BinOp p (:|:) q) = Set.union (purednf p) (purednf q)
      c _ = x
      x :: Set.Set (Set.Set formula)
      x = Set.singleton (Set.singleton (convertProp id fm)) :: Set.Set (Set.Set formula)

purecnf' :: forall formula atom. (PropositionalFormula formula atom) => formula -> Set.Set (Set.Set formula)
purecnf' fm =
    foldPropositional c (\ _ -> x) (\ _ -> x)  fm
    where
      c :: Combination formula -> Set.Set (Set.Set formula)
      c (BinOp p (:&:) q) = Set.union (purecnf' p) (purecnf' q)
      c (BinOp p (:|:) q) = Set.distrib (purecnf' p) (purecnf' q)
      c _ = x
      x :: Set.Set (Set.Set formula)
      x = Set.singleton (Set.singleton (convertProp id fm)) :: Set.Set (Set.Set formula)

-- ------------------------------------------------------------------------- 
-- Formula analog of list iterator "itlist".                                 
-- ------------------------------------------------------------------------- 

-- | Use this to implement foldAtoms
foldAtomsPropositional :: PropositionalFormula pf atom => (r -> atom -> r) -> r -> pf -> r
foldAtomsPropositional f i pf =
        foldPropositional co (const i) (f i) pf
        where
          co ((:~:) pf') = foldAtomsPropositional f i pf'
          co (BinOp p _ q) = foldAtomsPropositional f (foldAtomsPropositional f i q) p

-- | Deprecated - use foldAtoms.
overatoms :: forall formula atom r. PropositionalFormula formula atom => (atom -> r -> r) -> formula -> r -> r
overatoms f fm b = foldAtomsPropositional (flip f) b fm

mapAtomsPropositional :: forall formula atom. PropositionalFormula formula atom => (atom -> formula) -> formula -> formula
mapAtomsPropositional f fm =
    foldPropositional co tf at fm
    where
      co ((:~:) p) = mapAtomsPropositional f p
      co (BinOp p op q) = binop (mapAtomsPropositional f p) op (mapAtomsPropositional f q)
      tf flag = fromBool flag
      at x = f x

$(deriveSafeCopy 1 'base ''BinOp)
$(deriveSafeCopy 1 'base ''Combination)
