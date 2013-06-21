{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.Logic.Harrison.Herbrand where

import Control.Applicative.Error (Failing(..))
import Data.Logic.Classes.Atom (Atom(substitute, freeVariables))
import Data.Logic.Classes.FirstOrder (FirstOrderFormula)
import Data.Logic.Classes.Formula (Formula(..))
import Data.Logic.Classes.Literal.Literal (Literal)
import Data.Logic.Classes.Negate ((.~.))
import Data.Logic.Classes.Propositional (PropositionalFormula)
import Data.Logic.Classes.Term (fApp, Term)
import Data.Logic.Harrison.DP (dpll)
import Data.Logic.Harrison.FOL (generalize)
import Data.Logic.Harrison.Lib (allpairs, distrib')
import Data.Logic.Harrison.Normal (trivial)
import Data.Logic.Harrison.Prop (eval, simpcnf, simpdnf)
import Data.Logic.Harrison.Skolem (functions, runSkolem, skolemize)
import qualified Data.Map as Map (fromList, Map)
import qualified Data.Set as Set (empty, filter, fold, fromList, insert, map, minView, null, partition, Set, singleton, size, toList, union)
import Data.String (IsString(..))

{-
START_INTERACTIVE;;
let p36 = davisputnam'
 <<(forall x. exists y. P(x,y)) /\
   (forall x. exists y. G(x,y)) /\
   (forall x y. P(x,y) \/ G(x,y)
                ==> (forall z. P(y,z) \/ G(y,z) ==> H(x,z)))
   ==> (forall x. exists y. H(x,y))>>;;

let p29 = davisputnam'
 <<(exists x. P(x)) /\ (exists x. G(x)) ==>
   ((forall x. P(x) ==> H(x)) /\ (forall x. G(x) ==> J(x)) <=>
    (forall x y. P(x) /\ G(y) ==> H(x) /\ J(y)))>>;;
END_INTERACTIVE;;
-}
-- ========================================================================= 
-- Relation between FOL and propositonal logic; Herbrand theorem.            
--                                                                           
-- Copyright (c) 2003-2007, John Harrison. (See "LICENSE.txt" for details.)  
-- ========================================================================= 

-- ------------------------------------------------------------------------- 
-- Propositional valuation.                                                  
-- ------------------------------------------------------------------------- 

pholds :: (PropositionalFormula formula atom, Ord atom) => (Map.Map atom Bool) -> formula -> Bool
pholds d fm = eval fm d

-- ------------------------------------------------------------------------- 
-- Get the constants for Herbrand base, adding nullary one if necessary.     
-- ------------------------------------------------------------------------- 

herbfuns :: forall pf atom term v f. (PropositionalFormula pf atom, Formula pf atom, Atom atom term v, Term term v f, IsString f, Ord f) =>
            (atom -> Set.Set (f, Int))
         -> pf
         -> (Set.Set (f, Int), Set.Set (f, Int))
herbfuns fa fm =
  let (cns,fns) = Set.partition (\ (_,ar) -> ar == 0) (functions fa fm) in
  if Set.null cns then (Set.singleton (fromString "c",0),fns) else (cns,fns)

-- ------------------------------------------------------------------------- 
-- Enumeration of ground terms and m-tuples, ordered by total fns.           
-- ------------------------------------------------------------------------- 

groundterms :: forall term v f. (Term term v f) =>
               Set.Set term -> Set.Set (f, Int) -> Int -> Set.Set term
groundterms cntms _ 0 = cntms
groundterms cntms funcs n =
    Set.fold terms Set.empty funcs
    where
      terms (f,m) l = Set.union (Set.map (fApp f) (groundtuples cntms funcs (n - 1) m)) l

groundtuples :: forall term v f. (Term term v f) =>
                Set.Set term -> Set.Set (f, Int) -> Int -> Int -> Set.Set [term]
groundtuples _ _ 0 0 = Set.singleton []
groundtuples _ _ _ 0 = Set.empty
groundtuples cntms funcs n m =
    Set.fold tuples Set.empty (Set.fromList [0 .. n])
    where 
      tuples k l = Set.union (allpairs (:) (groundterms cntms funcs k) (groundtuples cntms funcs (n - k) (m - 1))) l

-- ------------------------------------------------------------------------- 
-- Iterate modifier "mfn" over ground terms till "tfn" fails.                
-- ------------------------------------------------------------------------- 

herbloop :: forall lit atom v term f. (Literal lit atom, Term term v f, Atom atom term v) =>
            (Set.Set (Set.Set lit) -> (lit -> lit) -> Set.Set (Set.Set lit) -> Set.Set (Set.Set lit))
         -> (Set.Set (Set.Set lit) -> Failing Bool)
         -> Set.Set (Set.Set lit)
         -> Set.Set term
         -> Set.Set (f, Int)
         -> [v]
         -> Int
         -> Set.Set (Set.Set lit)
         -> Set.Set [term]
         -> Set.Set [term]
         -> Failing (Set.Set [term])
herbloop mfn tfn fl0 cntms funcs fvs n fl tried tuples =
{-
  print_string(string_of_int(length tried) ++ " ground instances tried; " ++
               string_of_int(length fl) ++ " items in list")
  print_newline();
-}
  case Set.minView tuples of
    Nothing ->
          let newtups = groundtuples cntms funcs n (length fvs) in
          herbloop mfn tfn fl0 cntms funcs fvs (n + 1) fl tried newtups
    Just (tup, tups) ->
        let fpf' = Map.fromList (zip fvs tup) in
        let fl' = mfn fl0 (subst' fpf') fl in
        case tfn fl' of
          Failure msgs -> Failure msgs
          Success x ->
              if not x
              then Success (Set.insert tup tried)
              else herbloop mfn tfn fl0 cntms funcs fvs n fl' (Set.insert tup tried) tups

subst' :: forall lit atom term v f. (Literal lit atom, Atom atom term v, Term term v f) => Map.Map v term -> lit -> lit
subst' env fm =
    mapAtoms (atomic . substitute') fm
    where substitute' :: atom -> atom
          substitute' = substitute env

-- ------------------------------------------------------------------------- 
-- Hence a simple Gilmore-type procedure.                                    
-- ------------------------------------------------------------------------- 

gilmore_loop :: (Literal lit atom, Term term v f, Atom atom term v, Ord lit) =>
                Set.Set (Set.Set lit)
             -> Set.Set term
             -> Set.Set (f, Int)
             -> [v]
             -> Int
             -> Set.Set (Set.Set lit)
             -> Set.Set [term]
             -> Set.Set [term]
             -> Failing (Set.Set [term])
gilmore_loop =
    herbloop mfn (Success . not . Set.null)
    where
      mfn djs0 ifn djs = Set.filter (not . trivial) (distrib' (Set.map (Set.map ifn) djs0) djs)

gilmore :: forall fof pf atom term v f.
           (FirstOrderFormula fof atom v,
            PropositionalFormula pf atom,
            Literal pf atom,
            Term term v f,
            Atom atom term v,
            IsString f,
            Ord pf) =>
           (atom -> Set.Set (f, Int)) -> fof -> Failing Int
gilmore fa fm =
  let sfm = runSkolem (skolemize id ((.~.)(generalize fm))) :: pf in
  let fvs = Set.toList (foldAtoms (\ s (a :: atom) -> Set.union s (freeVariables a)) Set.empty sfm)
      (consts,funcs) = herbfuns fa sfm in
  let cntms = Set.map (\ (c,_) -> fApp c []) consts in
  gilmore_loop (simpdnf sfm :: Set.Set (Set.Set pf)) cntms funcs (fvs) 0 Set.empty Set.empty Set.empty >>= return . Set.size

-- ------------------------------------------------------------------------- 
-- First example and a little tracing.                                       
-- ------------------------------------------------------------------------- 
{-
test01 =
    let fm = exists "x" (for_all "y" (pApp "p" [vt "x"] .=>. pApp "p" [vt "y"]))
        sfm = skolemize ((.~.) fm) in
    TestList [TestCase (assertEqual "gilmore 1" 2 (gilmore fm))]

START_INTERACTIVE;;
gilmore <<exists x. forall y. P(x) ==> P(y)>>;;

let sfm = skolemize(Not <<exists x. forall y. P(x) ==> P(y)>>);;

-- ------------------------------------------------------------------------- 
-- Quick example.                                                            
-- ------------------------------------------------------------------------- 

let p24 = gilmore
 <<~(exists x. U(x) /\ Q(x)) /\
   (forall x. P(x) ==> Q(x) \/ R(x)) /\
   ~(exists x. P(x) ==> (exists x. Q(x))) /\
   (forall x. Q(x) /\ R(x) ==> U(x))
   ==> (exists x. P(x) /\ R(x))>>;;

-- ------------------------------------------------------------------------- 
-- Slightly less easy example.                                               
-- ------------------------------------------------------------------------- 

let p45 = gilmore
 <<(forall x. P(x) /\ (forall y. G(y) /\ H(x,y) ==> J(x,y))
              ==> (forall y. G(y) /\ H(x,y) ==> R(y))) /\
   ~(exists y. L(y) /\ R(y)) /\
   (exists x. P(x) /\ (forall y. H(x,y) ==> L(y)) /\
                      (forall y. G(y) /\ H(x,y) ==> J(x,y)))
   ==> (exists x. P(x) /\ ~(exists y. G(y) /\ H(x,y)))>>;;
END_INTERACTIVE;;
-}
-- ------------------------------------------------------------------------- 
-- Apparently intractable example.                                           
-- ------------------------------------------------------------------------- 

{-

let p20 = gilmore
 <<(forall x y. exists z. forall w. P(x) /\ Q(y) ==> R(z) /\ U(w))
   ==> (exists x y. P(x) /\ Q(y)) ==> (exists z. R(z))>>;;

-}


-- ------------------------------------------------------------------------- 
-- The Davis-Putnam procedure for first order logic.                         
-- ------------------------------------------------------------------------- 

dp_mfn :: (Ord b, Ord a) =>
          Set.Set (Set.Set a)
       -> (a -> b)
       -> Set.Set (Set.Set b)
       -> Set.Set (Set.Set b)
dp_mfn cjs0 ifn cjs = Set.union (Set.map (Set.map ifn) cjs0) cjs

dp_loop :: forall lit atom v term f. (Literal lit atom, Term term v f, Atom atom term v, Ord lit) =>
           Set.Set (Set.Set lit)
        -> Set.Set term
        -> Set.Set (f, Int)
        -> [v]
        -> Int
        -> Set.Set (Set.Set lit)
        -> Set.Set [term]
        -> Set.Set [term]
        -> Failing (Set.Set [term])
dp_loop = herbloop dp_mfn dpll

davisputnam :: forall fof atom term v lit f.
               (FirstOrderFormula fof atom v,
                PropositionalFormula lit atom,
                Literal lit atom,
                Term term v f,
                Atom atom term v,
                IsString f,
                Ord lit) =>
               (atom -> Set.Set (f, Int)) -> fof -> Failing Int
davisputnam fa fm =
  let (sfm :: lit) = runSkolem (skolemize id ((.~.)(generalize fm))) in
  let fvs = Set.toList (foldAtoms (\ s (a :: atom) -> Set.union (freeVariables a) s) Set.empty sfm)
      (consts,funcs) = herbfuns fa sfm in
  let cntms = Set.map (\ (c,_) -> fApp c [] :: term) consts in
  dp_loop (simpcnf sfm :: Set.Set (Set.Set lit)) cntms funcs fvs 0 Set.empty Set.empty Set.empty >>= return . Set.size

-- ------------------------------------------------------------------------- 
-- Show how much better than the Gilmore procedure this can be.              
-- ------------------------------------------------------------------------- 

{-
START_INTERACTIVE;;
let p20 = davisputnam
 <<(forall x y. exists z. forall w. P(x) /\ Q(y) ==> R(z) /\ U(w))
   ==> (exists x y. P(x) /\ Q(y)) ==> (exists z. R(z))>>;;
END_INTERACTIVE;;
-}

-- ------------------------------------------------------------------------- 
-- Try to cut out useless instantiations in final result.                    
-- ------------------------------------------------------------------------- 

dp_refine :: (Literal lit atom, Atom atom term v, Term term v f) =>
             Set.Set (Set.Set lit) -> [v] -> Set.Set [term] -> Set.Set [term] -> Failing (Set.Set [term])
dp_refine cjs0 fvs dknow need =
    case Set.minView dknow of
      Nothing -> Success need
      Just (cl, dknow') ->
          let mfn = dp_mfn cjs0 . subst' . Map.fromList . zip fvs in
          dpll (Set.fold mfn Set.empty (Set.union need dknow')) >>= \ flag ->
          if flag then return (Set.insert cl need) else return need >>=
          dp_refine cjs0 fvs dknow'

dp_refine_loop :: forall lit atom term v f. (Literal lit atom, Term term v f, Atom atom term v) =>
                  Set.Set (Set.Set lit)
               -> Set.Set term
               -> Set.Set (f, Int)
               -> [v]
               -> Int
               -> Set.Set (Set.Set lit)
               -> Set.Set [term]
               -> Set.Set [term]
               -> Failing (Set.Set [term])
dp_refine_loop cjs0 cntms funcs fvs n cjs tried tuples =
    dp_loop cjs0 cntms funcs fvs n cjs tried tuples >>= \ tups ->
    dp_refine cjs0 fvs tups Set.empty

-- ------------------------------------------------------------------------- 
-- Show how few of the instances we really need. Hence unification!          
-- ------------------------------------------------------------------------- 

davisputnam' :: forall fof atom term lit v f pf.
                (FirstOrderFormula fof atom v,
                 Literal lit atom,
                 PropositionalFormula pf atom, -- Formula pf atom,
                 Term term v f,
                 Atom atom term v,
                 IsString f) =>
                (atom -> Set.Set (f, Int)) -> fof -> Failing Int
davisputnam' fa fm =
    let (sfm :: pf) = runSkolem (skolemize id ((.~.)(generalize fm))) in
    let fvs = Set.toList (foldAtoms (\ s (a :: atom) -> Set.union (freeVariables a) s) Set.empty sfm)
        (consts,funcs) = herbfuns fa sfm in
    let cntms = Set.map (\ (c,_) -> fApp c []) consts in
    dp_refine_loop (simpcnf sfm :: Set.Set (Set.Set lit)) cntms funcs fvs 0 Set.empty Set.empty Set.empty >>= return . Set.size

