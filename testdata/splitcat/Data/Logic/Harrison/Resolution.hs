{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Data.Logic.Harrison.Resolution
    ( resolution1
    , resolution2
    , resolution3
    , presolution
    , matchAtomsEq
    ) where

import Data.Logic.Classes.Atom (Atom(match))
import Data.Logic.Classes.Combine (Combination(..))
import Data.Logic.Classes.Equals (AtomEq, zipAtomsEq)
import Data.Logic.Classes.FirstOrder (FirstOrderFormula(..), zipFirstOrder)
import Data.Logic.Classes.Literal (Literal)
import Data.Logic.Classes.Negate ((.~.), positive)
import Data.Logic.Classes.Propositional (PropositionalFormula)
import Data.Logic.Classes.Term (Term(vt, foldTerm))
import Data.Logic.Classes.Variable (Variable(prefix))
import Data.Logic.Failing (failing, Failing(..))
import Data.Logic.Harrison.FOL (fv, generalize, list_conj, list_disj, subst)
import Data.Logic.Harrison.Lib (allnonemptysubsets, allpairs, allsubsets, apply, defined, setAll, setAny, settryfind, (|->))
import Data.Logic.Harrison.Normal (simpcnf, simpdnf, trivial)
import Data.Logic.Harrison.Skolem (askolemize, pnf, SkolemT, specialize)
import Data.Logic.Harrison.Tableaux (unify_literals)
import Data.Logic.Harrison.Unif (solve)
import qualified Data.Map as Map (empty, fromList, Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set (difference, empty, filter, fold, insert, map, member, minView, null, partition, Set, singleton, toList, union)

{-
-- ------------------------------------------------------------------------- 
-- The Pelletier examples again.                                             
-- ------------------------------------------------------------------------- 

{- **********

let p1 = time presolution
 <<p ==> q <=> ~q ==> ~p>>;;

let p2 = time presolution
 <<~ ~p <=> p>>;;

let p3 = time presolution
 <<~(p ==> q) ==> q ==> p>>;;

let p4 = time presolution
 <<~p ==> q <=> ~q ==> p>>;;

let p5 = time presolution
 <<(p \/ q ==> p \/ r) ==> p \/ (q ==> r)>>;;

let p6 = time presolution
 <<p \/ ~p>>;;

let p7 = time presolution
 <<p \/ ~ ~ ~p>>;;

let p8 = time presolution
 <<((p ==> q) ==> p) ==> p>>;;

let p9 = time presolution
 <<(p \/ q) /\ (~p \/ q) /\ (p \/ ~q) ==> ~(~q \/ ~q)>>;;

let p10 = time presolution
 <<(q ==> r) /\ (r ==> p /\ q) /\ (p ==> q /\ r) ==> (p <=> q)>>;;

let p11 = time presolution
 <<p <=> p>>;;

let p12 = time presolution
 <<((p <=> q) <=> r) <=> (p <=> (q <=> r))>>;;

let p13 = time presolution
 <<p \/ q /\ r <=> (p \/ q) /\ (p \/ r)>>;;

let p14 = time presolution
 <<(p <=> q) <=> (q \/ ~p) /\ (~q \/ p)>>;;

let p15 = time presolution
 <<p ==> q <=> ~p \/ q>>;;

let p16 = time presolution
 <<(p ==> q) \/ (q ==> p)>>;;

let p17 = time presolution
 <<p /\ (q ==> r) ==> s <=> (~p \/ q \/ s) /\ (~p \/ ~r \/ s)>>;;

-- ------------------------------------------------------------------------- 
-- Monadic Predicate Logic.                                                  
-- ------------------------------------------------------------------------- 

let p18 = time presolution
 <<exists y. forall x. P(y) ==> P(x)>>;;

let p19 = time presolution
 <<exists x. forall y z. (P(y) ==> Q(z)) ==> P(x) ==> Q(x)>>;;

let p20 = time presolution
 <<(forall x y. exists z. forall w. P(x) /\ Q(y) ==> R(z) /\ U(w))
   ==> (exists x y. P(x) /\ Q(y)) ==> (exists z. R(z))>>;;

let p21 = time presolution
 <<(exists x. P ==> Q(x)) /\ (exists x. Q(x) ==> P)
   ==> (exists x. P <=> Q(x))>>;;

let p22 = time presolution
 <<(forall x. P <=> Q(x)) ==> (P <=> (forall x. Q(x)))>>;;

let p23 = time presolution
 <<(forall x. P \/ Q(x)) <=> P \/ (forall x. Q(x))>>;;

let p24 = time presolution
 <<~(exists x. U(x) /\ Q(x)) /\
   (forall x. P(x) ==> Q(x) \/ R(x)) /\
   ~(exists x. P(x) ==> (exists x. Q(x))) /\
   (forall x. Q(x) /\ R(x) ==> U(x)) ==>
   (exists x. P(x) /\ R(x))>>;;

let p25 = time presolution
 <<(exists x. P(x)) /\
   (forall x. U(x) ==> ~G(x) /\ R(x)) /\
   (forall x. P(x) ==> G(x) /\ U(x)) /\
   ((forall x. P(x) ==> Q(x)) \/ (exists x. Q(x) /\ P(x))) ==>
   (exists x. Q(x) /\ P(x))>>;;

let p26 = time presolution
 <<((exists x. P(x)) <=> (exists x. Q(x))) /\
   (forall x y. P(x) /\ Q(y) ==> (R(x) <=> U(y))) ==>
   ((forall x. P(x) ==> R(x)) <=> (forall x. Q(x) ==> U(x)))>>;;

let p27 = time presolution
 <<(exists x. P(x) /\ ~Q(x)) /\
   (forall x. P(x) ==> R(x)) /\
   (forall x. U(x) /\ V(x) ==> P(x)) /\
   (exists x. R(x) /\ ~Q(x)) ==>
   (forall x. U(x) ==> ~R(x)) ==>
   (forall x. U(x) ==> ~V(x))>>;;

let p28 = time presolution
 <<(forall x. P(x) ==> (forall x. Q(x))) /\
   ((forall x. Q(x) \/ R(x)) ==> (exists x. Q(x) /\ R(x))) /\
   ((exists x. R(x)) ==> (forall x. L(x) ==> M(x))) ==>
   (forall x. P(x) /\ L(x) ==> M(x))>>;;

let p29 = time presolution
 <<(exists x. P(x)) /\ (exists x. G(x)) ==>
   ((forall x. P(x) ==> H(x)) /\ (forall x. G(x) ==> J(x)) <=>
    (forall x y. P(x) /\ G(y) ==> H(x) /\ J(y)))>>;;

let p30 = time presolution
 <<(forall x. P(x) \/ G(x) ==> ~H(x)) /\
   (forall x. (G(x) ==> ~U(x)) ==> P(x) /\ H(x)) ==>
   (forall x. U(x))>>;;

let p31 = time presolution
 <<~(exists x. P(x) /\ (G(x) \/ H(x))) /\ (exists x. Q(x) /\ P(x)) /\
   (forall x. ~H(x) ==> J(x)) ==>
   (exists x. Q(x) /\ J(x))>>;;

let p32 = time presolution
 <<(forall x. P(x) /\ (G(x) \/ H(x)) ==> Q(x)) /\
   (forall x. Q(x) /\ H(x) ==> J(x)) /\
   (forall x. R(x) ==> H(x)) ==>
   (forall x. P(x) /\ R(x) ==> J(x))>>;;

let p33 = time presolution
 <<(forall x. P(a) /\ (P(x) ==> P(b)) ==> P(c)) <=>
   (forall x. P(a) ==> P(x) \/ P(c)) /\ (P(a) ==> P(b) ==> P(c))>>;;

let p34 = time presolution
 <<((exists x. forall y. P(x) <=> P(y)) <=>
    ((exists x. Q(x)) <=> (forall y. Q(y)))) <=>
   ((exists x. forall y. Q(x) <=> Q(y)) <=>
    ((exists x. P(x)) <=> (forall y. P(y))))>>;;

let p35 = time presolution
 <<exists x y. P(x,y) ==> (forall x y. P(x,y))>>;;

-- ------------------------------------------------------------------------- 
--  Full predicate logic (without Identity and Functions)                    
-- ------------------------------------------------------------------------- 

let p36 = time presolution
 <<(forall x. exists y. P(x,y)) /\
   (forall x. exists y. G(x,y)) /\
   (forall x y. P(x,y) \/ G(x,y)
   ==> (forall z. P(y,z) \/ G(y,z) ==> H(x,z)))
       ==> (forall x. exists y. H(x,y))>>;;

let p37 = time presolution
 <<(forall z.
     exists w. forall x. exists y. (P(x,z) ==> P(y,w)) /\ P(y,z) /\
     (P(y,w) ==> (exists u. Q(u,w)))) /\
   (forall x z. ~P(x,z) ==> (exists y. Q(y,z))) /\
   ((exists x y. Q(x,y)) ==> (forall x. R(x,x))) ==>
   (forall x. exists y. R(x,y))>>;;

{- ** This one seems too slow

let p38 = time presolution
 <<(forall x.
     P(a) /\ (P(x) ==> (exists y. P(y) /\ R(x,y))) ==>
     (exists z w. P(z) /\ R(x,w) /\ R(w,z))) <=>
   (forall x.
     (~P(a) \/ P(x) \/ (exists z w. P(z) /\ R(x,w) /\ R(w,z))) /\
     (~P(a) \/ ~(exists y. P(y) /\ R(x,y)) \/
     (exists z w. P(z) /\ R(x,w) /\ R(w,z))))>>;;

 ** -}

let p39 = time presolution
 <<~(exists x. forall y. P(y,x) <=> ~P(y,y))>>;;

let p40 = time presolution
 <<(exists y. forall x. P(x,y) <=> P(x,x))
  ==> ~(forall x. exists y. forall z. P(z,y) <=> ~P(z,x))>>;;

let p41 = time presolution
 <<(forall z. exists y. forall x. P(x,y) <=> P(x,z) /\ ~P(x,x))
  ==> ~(exists z. forall x. P(x,z))>>;;

{- ** Also very slow

let p42 = time presolution
 <<~(exists y. forall x. P(x,y) <=> ~(exists z. P(x,z) /\ P(z,x)))>>;;

 ** -}

{- ** and this one too..

let p43 = time presolution
 <<(forall x y. Q(x,y) <=> forall z. P(z,x) <=> P(z,y))
   ==> forall x y. Q(x,y) <=> Q(y,x)>>;;

 ** -}

let p44 = time presolution
 <<(forall x. P(x) ==> (exists y. G(y) /\ H(x,y)) /\
   (exists y. G(y) /\ ~H(x,y))) /\
   (exists x. J(x) /\ (forall y. G(y) ==> H(x,y))) ==>
   (exists x. J(x) /\ ~P(x))>>;;

{- ** and this...

let p45 = time presolution
 <<(forall x.
     P(x) /\ (forall y. G(y) /\ H(x,y) ==> J(x,y)) ==>
       (forall y. G(y) /\ H(x,y) ==> R(y))) /\
   ~(exists y. L(y) /\ R(y)) /\
   (exists x. P(x) /\ (forall y. H(x,y) ==>
     L(y)) /\ (forall y. G(y) /\ H(x,y) ==> J(x,y))) ==>
   (exists x. P(x) /\ ~(exists y. G(y) /\ H(x,y)))>>;;

 ** -}

{- ** and this

let p46 = time presolution
 <<(forall x. P(x) /\ (forall y. P(y) /\ H(y,x) ==> G(y)) ==> G(x)) /\
   ((exists x. P(x) /\ ~G(x)) ==>
    (exists x. P(x) /\ ~G(x) /\
               (forall y. P(y) /\ ~G(y) ==> J(x,y)))) /\
   (forall x y. P(x) /\ P(y) /\ H(x,y) ==> ~J(y,x)) ==>
   (forall x. P(x) ==> G(x))>>;;

 ** -}

-- ------------------------------------------------------------------------- 
-- Example from Manthey and Bry, CADE-9.                                     
-- ------------------------------------------------------------------------- 

let p55 = time presolution
 <<lives(agatha) /\ lives(butler) /\ lives(charles) /\
   (killed(agatha,agatha) \/ killed(butler,agatha) \/
    killed(charles,agatha)) /\
   (forall x y. killed(x,y) ==> hates(x,y) /\ ~richer(x,y)) /\
   (forall x. hates(agatha,x) ==> ~hates(charles,x)) /\
   (hates(agatha,agatha) /\ hates(agatha,charles)) /\
   (forall x. lives(x) /\ ~richer(x,agatha) ==> hates(butler,x)) /\
   (forall x. hates(agatha,x) ==> hates(butler,x)) /\
   (forall x. ~hates(x,agatha) \/ ~hates(x,butler) \/ ~hates(x,charles))
   ==> killed(agatha,agatha) /\
       ~killed(butler,agatha) /\
       ~killed(charles,agatha)>>;;

let p57 = time presolution
 <<P(f((a),b),f(b,c)) /\
   P(f(b,c),f(a,c)) /\
   (forall (x) y z. P(x,y) /\ P(y,z) ==> P(x,z))
   ==> P(f(a,b),f(a,c))>>;;

-- ------------------------------------------------------------------------- 
-- See info-hol, circa 1500.                                                 
-- ------------------------------------------------------------------------- 

let p58 = time presolution
 <<forall P Q R. forall x. exists v. exists w. forall y. forall z.
    ((P(x) /\ Q(y)) ==> ((P(v) \/ R(w))  /\ (R(z) ==> Q(v))))>>;;

let p59 = time presolution
 <<(forall x. P(x) <=> ~P(f(x))) ==> (exists x. P(x) /\ ~P(f(x)))>>;;

let p60 = time presolution
 <<forall x. P(x,f(x)) <=>
            exists y. (forall z. P(z,y) ==> P(z,f(x))) /\ P(x,y)>>;;

-- ------------------------------------------------------------------------- 
-- From Gilmore's classic paper.                                             
-- ------------------------------------------------------------------------- 

let gilmore_1 = time presolution
 <<exists x. forall y z.
      ((F(y) ==> G(y)) <=> F(x)) /\
      ((F(y) ==> H(y)) <=> G(x)) /\
      (((F(y) ==> G(y)) ==> H(y)) <=> H(x))
      ==> F(z) /\ G(z) /\ H(z)>>;;

{- ** This is not valid, according to Gilmore

let gilmore_2 = time presolution
 <<exists x y. forall z.
        (F(x,z) <=> F(z,y)) /\ (F(z,y) <=> F(z,z)) /\ (F(x,y) <=> F(y,x))
        ==> (F(x,y) <=> F(x,z))>>;;

 ** -}

let gilmore_3 = time presolution
 <<exists x. forall y z.
        ((F(y,z) ==> (G(y) ==> H(x))) ==> F(x,x)) /\
        ((F(z,x) ==> G(x)) ==> H(z)) /\
        F(x,y)
        ==> F(z,z)>>;;

let gilmore_4 = time presolution
 <<exists x y. forall z.
        (F(x,y) ==> F(y,z) /\ F(z,z)) /\
        (F(x,y) /\ G(x,y) ==> G(x,z) /\ G(z,z))>>;;

let gilmore_5 = time presolution
 <<(forall x. exists y. F(x,y) \/ F(y,x)) /\
   (forall x y. F(y,x) ==> F(y,y))
   ==> exists z. F(z,z)>>;;

let gilmore_6 = time presolution
 <<forall x. exists y.
        (exists u. forall v. F(u,x) ==> G(v,u) /\ G(u,x))
        ==> (exists u. forall v. F(u,y) ==> G(v,u) /\ G(u,y)) \/
            (forall u v. exists w. G(v,u) \/ H(w,y,u) ==> G(u,w))>>;;

let gilmore_7 = time presolution
 <<(forall x. K(x) ==> exists y. L(y) /\ (F(x,y) ==> G(x,y))) /\
   (exists z. K(z) /\ forall u. L(u) ==> F(z,u))
   ==> exists v w. K(v) /\ L(w) /\ G(v,w)>>;;

let gilmore_8 = time presolution
 <<exists x. forall y z.
        ((F(y,z) ==> (G(y) ==> (forall u. exists v. H(u,v,x)))) ==> F(x,x)) /\
        ((F(z,x) ==> G(x)) ==> (forall u. exists v. H(u,v,z))) /\
        F(x,y)
        ==> F(z,z)>>;;

{- ** This one still isn't easy!

let gilmore_9 = time presolution
 <<forall x. exists y. forall z.
        ((forall u. exists v. F(y,u,v) /\ G(y,u) /\ ~H(y,x))
          ==> (forall u. exists v. F(x,u,v) /\ G(z,u) /\ ~H(x,z))
             ==> (forall u. exists v. F(x,u,v) /\ G(y,u) /\ ~H(x,y))) /\
        ((forall u. exists v. F(x,u,v) /\ G(y,u) /\ ~H(x,y))
         ==> ~(forall u. exists v. F(x,u,v) /\ G(z,u) /\ ~H(x,z))
             ==> (forall u. exists v. F(y,u,v) /\ G(y,u) /\ ~H(y,x)) /\
                 (forall u. exists v. F(z,u,v) /\ G(y,u) /\ ~H(z,y)))>>;;

 ** -}

-- ------------------------------------------------------------------------- 
-- Example from Davis-Putnam papers where Gilmore procedure is poor.         
-- ------------------------------------------------------------------------- 

let davis_putnam_example = time presolution
 <<exists x. exists y. forall z.
        (F(x,y) ==> (F(y,z) /\ F(z,z))) /\
        ((F(x,y) /\ G(x,y)) ==> (G(x,z) /\ G(z,z)))>>;;

*********** -}
END_INTERACTIVE;;

-- ------------------------------------------------------------------------- 
-- Example                                                                   
-- ------------------------------------------------------------------------- 

START_INTERACTIVE;;
let gilmore_1 = resolution
 <<exists x. forall y z.
      ((F(y) ==> G(y)) <=> F(x)) /\
      ((F(y) ==> H(y)) <=> G(x)) /\
      (((F(y) ==> G(y)) ==> H(y)) <=> H(x))
      ==> F(z) /\ G(z) /\ H(z)>>;;

-- ------------------------------------------------------------------------- 
-- Pelletiers yet again.                                                     
-- ------------------------------------------------------------------------- 

{- ************

let p1 = time resolution
 <<p ==> q <=> ~q ==> ~p>>;;

let p2 = time resolution
 <<~ ~p <=> p>>;;

let p3 = time resolution
 <<~(p ==> q) ==> q ==> p>>;;

let p4 = time resolution
 <<~p ==> q <=> ~q ==> p>>;;

let p5 = time resolution
 <<(p \/ q ==> p \/ r) ==> p \/ (q ==> r)>>;;

let p6 = time resolution
 <<p \/ ~p>>;;

let p7 = time resolution
 <<p \/ ~ ~ ~p>>;;

let p8 = time resolution
 <<((p ==> q) ==> p) ==> p>>;;

let p9 = time resolution
 <<(p \/ q) /\ (~p \/ q) /\ (p \/ ~q) ==> ~(~q \/ ~q)>>;;

let p10 = time resolution
 <<(q ==> r) /\ (r ==> p /\ q) /\ (p ==> q /\ r) ==> (p <=> q)>>;;

let p11 = time resolution
 <<p <=> p>>;;

let p12 = time resolution
 <<((p <=> q) <=> r) <=> (p <=> (q <=> r))>>;;

let p13 = time resolution
 <<p \/ q /\ r <=> (p \/ q) /\ (p \/ r)>>;;

let p14 = time resolution
 <<(p <=> q) <=> (q \/ ~p) /\ (~q \/ p)>>;;

let p15 = time resolution
 <<p ==> q <=> ~p \/ q>>;;

let p16 = time resolution
 <<(p ==> q) \/ (q ==> p)>>;;

let p17 = time resolution
 <<p /\ (q ==> r) ==> s <=> (~p \/ q \/ s) /\ (~p \/ ~r \/ s)>>;;

(* ------------------------------------------------------------------------- *)
(* Monadic Predicate Logic.                                                  *)
(* ------------------------------------------------------------------------- *)

let p18 = time resolution
 <<exists y. forall x. P(y) ==> P(x)>>;;

let p19 = time resolution
 <<exists x. forall y z. (P(y) ==> Q(z)) ==> P(x) ==> Q(x)>>;;

let p20 = time resolution
 <<(forall x y. exists z. forall w. P(x) /\ Q(y) ==> R(z) /\ U(w)) ==>
   (exists x y. P(x) /\ Q(y)) ==>
   (exists z. R(z))>>;;

let p21 = time resolution
 <<(exists x. P ==> Q(x)) /\ (exists x. Q(x) ==> P) ==> (exists x. P <=> Q(x))>>;;

let p22 = time resolution
 <<(forall x. P <=> Q(x)) ==> (P <=> (forall x. Q(x)))>>;;

let p23 = time resolution
 <<(forall x. P \/ Q(x)) <=> P \/ (forall x. Q(x))>>;;

let p24 = time resolution
 <<~(exists x. U(x) /\ Q(x)) /\
   (forall x. P(x) ==> Q(x) \/ R(x)) /\
   ~(exists x. P(x) ==> (exists x. Q(x))) /\
   (forall x. Q(x) /\ R(x) ==> U(x)) ==>
   (exists x. P(x) /\ R(x))>>;;

let p25 = time resolution
 <<(exists x. P(x)) /\
   (forall x. U(x) ==> ~G(x) /\ R(x)) /\
   (forall x. P(x) ==> G(x) /\ U(x)) /\
   ((forall x. P(x) ==> Q(x)) \/ (exists x. Q(x) /\ P(x))) ==>
   (exists x. Q(x) /\ P(x))>>;;

let p26 = time resolution
 <<((exists x. P(x)) <=> (exists x. Q(x))) /\
   (forall x y. P(x) /\ Q(y) ==> (R(x) <=> U(y))) ==>
   ((forall x. P(x) ==> R(x)) <=> (forall x. Q(x) ==> U(x)))>>;;

let p27 = time resolution
 <<(exists x. P(x) /\ ~Q(x)) /\
   (forall x. P(x) ==> R(x)) /\
   (forall x. U(x) /\ V(x) ==> P(x)) /\
   (exists x. R(x) /\ ~Q(x)) ==>
   (forall x. U(x) ==> ~R(x)) ==>
   (forall x. U(x) ==> ~V(x))>>;;

let p28 = time resolution
 <<(forall x. P(x) ==> (forall x. Q(x))) /\
   ((forall x. Q(x) \/ R(x)) ==> (exists x. Q(x) /\ R(x))) /\
   ((exists x. R(x)) ==> (forall x. L(x) ==> M(x))) ==>
   (forall x. P(x) /\ L(x) ==> M(x))>>;;

let p29 = time resolution
 <<(exists x. P(x)) /\ (exists x. G(x)) ==>
   ((forall x. P(x) ==> H(x)) /\ (forall x. G(x) ==> J(x)) <=>
    (forall x y. P(x) /\ G(y) ==> H(x) /\ J(y)))>>;;

let p30 = time resolution
 <<(forall x. P(x) \/ G(x) ==> ~H(x)) /\ (forall x. (G(x) ==> ~U(x)) ==>
     P(x) /\ H(x)) ==>
   (forall x. U(x))>>;;

let p31 = time resolution
 <<~(exists x. P(x) /\ (G(x) \/ H(x))) /\ (exists x. Q(x) /\ P(x)) /\
   (forall x. ~H(x) ==> J(x)) ==>
   (exists x. Q(x) /\ J(x))>>;;

let p32 = time resolution
 <<(forall x. P(x) /\ (G(x) \/ H(x)) ==> Q(x)) /\
   (forall x. Q(x) /\ H(x) ==> J(x)) /\
   (forall x. R(x) ==> H(x)) ==>
   (forall x. P(x) /\ R(x) ==> J(x))>>;;

let p33 = time resolution
 <<(forall x. P(a) /\ (P(x) ==> P(b)) ==> P(c)) <=>
   (forall x. P(a) ==> P(x) \/ P(c)) /\ (P(a) ==> P(b) ==> P(c))>>;;

let p34 = time resolution
 <<((exists x. forall y. P(x) <=> P(y)) <=>
   ((exists x. Q(x)) <=> (forall y. Q(y)))) <=>
   ((exists x. forall y. Q(x) <=> Q(y)) <=>
  ((exists x. P(x)) <=> (forall y. P(y))))>>;;

let p35 = time resolution
 <<exists x y. P(x,y) ==> (forall x y. P(x,y))>>;;

(* ------------------------------------------------------------------------- *)
(*  Full predicate logic (without Identity and Functions)                    *)
(* ------------------------------------------------------------------------- *)

let p36 = time resolution
 <<(forall x. exists y. P(x,y)) /\
   (forall x. exists y. G(x,y)) /\
   (forall x y. P(x,y) \/ G(x,y)
   ==> (forall z. P(y,z) \/ G(y,z) ==> H(x,z)))
       ==> (forall x. exists y. H(x,y))>>;;

let p37 = time resolution
 <<(forall z.
     exists w. forall x. exists y. (P(x,z) ==> P(y,w)) /\ P(y,z) /\
     (P(y,w) ==> (exists u. Q(u,w)))) /\
   (forall x z. ~P(x,z) ==> (exists y. Q(y,z))) /\
   ((exists x y. Q(x,y)) ==> (forall x. R(x,x))) ==>
   (forall x. exists y. R(x,y))>>;;

(*** This one seems too slow

let p38 = time resolution
 <<(forall x.
     P(a) /\ (P(x) ==> (exists y. P(y) /\ R(x,y))) ==>
     (exists z w. P(z) /\ R(x,w) /\ R(w,z))) <=>
   (forall x.
     (~P(a) \/ P(x) \/ (exists z w. P(z) /\ R(x,w) /\ R(w,z))) /\
     (~P(a) \/ ~(exists y. P(y) /\ R(x,y)) \/
     (exists z w. P(z) /\ R(x,w) /\ R(w,z))))>>;;

 ***)

let p39 = time resolution
 <<~(exists x. forall y. P(y,x) <=> ~P(y,y))>>;;

let p40 = time resolution
 <<(exists y. forall x. P(x,y) <=> P(x,x))
  ==> ~(forall x. exists y. forall z. P(z,y) <=> ~P(z,x))>>;;

let p41 = time resolution
 <<(forall z. exists y. forall x. P(x,y) <=> P(x,z) /\ ~P(x,x))
  ==> ~(exists z. forall x. P(x,z))>>;;

(*** Also very slow

let p42 = time resolution
 <<~(exists y. forall x. P(x,y) <=> ~(exists z. P(x,z) /\ P(z,x)))>>;;

 ***)

(*** and this one too..

let p43 = time resolution
 <<(forall x y. Q(x,y) <=> forall z. P(z,x) <=> P(z,y))
   ==> forall x y. Q(x,y) <=> Q(y,x)>>;;

 ***)

let p44 = time resolution
 <<(forall x. P(x) ==> (exists y. G(y) /\ H(x,y)) /\
   (exists y. G(y) /\ ~H(x,y))) /\
   (exists x. J(x) /\ (forall y. G(y) ==> H(x,y))) ==>
   (exists x. J(x) /\ ~P(x))>>;;

(*** and this...

let p45 = time resolution
 <<(forall x.
     P(x) /\ (forall y. G(y) /\ H(x,y) ==> J(x,y)) ==>
       (forall y. G(y) /\ H(x,y) ==> R(y))) /\
   ~(exists y. L(y) /\ R(y)) /\
   (exists x. P(x) /\ (forall y. H(x,y) ==>
     L(y)) /\ (forall y. G(y) /\ H(x,y) ==> J(x,y))) ==>
   (exists x. P(x) /\ ~(exists y. G(y) /\ H(x,y)))>>;;

 ***)

(*** and this

let p46 = time resolution
 <<(forall x. P(x) /\ (forall y. P(y) /\ H(y,x) ==> G(y)) ==> G(x)) /\
   ((exists x. P(x) /\ ~G(x)) ==>
    (exists x. P(x) /\ ~G(x) /\
               (forall y. P(y) /\ ~G(y) ==> J(x,y)))) /\
   (forall x y. P(x) /\ P(y) /\ H(x,y) ==> ~J(y,x)) ==>
   (forall x. P(x) ==> G(x))>>;;

 ***)

(* ------------------------------------------------------------------------- *)
(* Example from Manthey and Bry, CADE-9.                                     *)
(* ------------------------------------------------------------------------- *)

let p55 = time resolution
 <<lives(agatha) /\ lives(butler) /\ lives(charles) /\
   (killed(agatha,agatha) \/ killed(butler,agatha) \/
    killed(charles,agatha)) /\
   (forall x y. killed(x,y) ==> hates(x,y) /\ ~richer(x,y)) /\
   (forall x. hates(agatha,x) ==> ~hates(charles,x)) /\
   (hates(agatha,agatha) /\ hates(agatha,charles)) /\
   (forall x. lives(x) /\ ~richer(x,agatha) ==> hates(butler,x)) /\
   (forall x. hates(agatha,x) ==> hates(butler,x)) /\
   (forall x. ~hates(x,agatha) \/ ~hates(x,butler) \/ ~hates(x,charles))
   ==> killed(agatha,agatha) /\
       ~killed(butler,agatha) /\
       ~killed(charles,agatha)>>;;

let p57 = time resolution
 <<P(f((a),b),f(b,c)) /\
   P(f(b,c),f(a,c)) /\
   (forall (x) y z. P(x,y) /\ P(y,z) ==> P(x,z))
   ==> P(f(a,b),f(a,c))>>;;

(* ------------------------------------------------------------------------- *)
(* See info-hol, circa 1500.                                                 *)
(* ------------------------------------------------------------------------- *)

let p58 = time resolution
 <<forall P Q R. forall x. exists v. exists w. forall y. forall z.
    ((P(x) /\ Q(y)) ==> ((P(v) \/ R(w))  /\ (R(z) ==> Q(v))))>>;;

let p59 = time resolution
 <<(forall x. P(x) <=> ~P(f(x))) ==> (exists x. P(x) /\ ~P(f(x)))>>;;

let p60 = time resolution
 <<forall x. P(x,f(x)) <=>
            exists y. (forall z. P(z,y) ==> P(z,f(x))) /\ P(x,y)>>;;

(* ------------------------------------------------------------------------- *)
(* From Gilmore's classic paper.                                             *)
(* ------------------------------------------------------------------------- *)

let gilmore_1 = time resolution
 <<exists x. forall y z.
      ((F(y) ==> G(y)) <=> F(x)) /\
      ((F(y) ==> H(y)) <=> G(x)) /\
      (((F(y) ==> G(y)) ==> H(y)) <=> H(x))
      ==> F(z) /\ G(z) /\ H(z)>>;;

(*** This is not valid, according to Gilmore

let gilmore_2 = time resolution
 <<exists x y. forall z.
        (F(x,z) <=> F(z,y)) /\ (F(z,y) <=> F(z,z)) /\ (F(x,y) <=> F(y,x))
        ==> (F(x,y) <=> F(x,z))>>;;

 ***)

let gilmore_3 = time resolution
 <<exists x. forall y z.
        ((F(y,z) ==> (G(y) ==> H(x))) ==> F(x,x)) /\
        ((F(z,x) ==> G(x)) ==> H(z)) /\
        F(x,y)
        ==> F(z,z)>>;;

let gilmore_4 = time resolution
 <<exists x y. forall z.
        (F(x,y) ==> F(y,z) /\ F(z,z)) /\
        (F(x,y) /\ G(x,y) ==> G(x,z) /\ G(z,z))>>;;

let gilmore_5 = time resolution
 <<(forall x. exists y. F(x,y) \/ F(y,x)) /\
   (forall x y. F(y,x) ==> F(y,y))
   ==> exists z. F(z,z)>>;;

let gilmore_6 = time resolution
 <<forall x. exists y.
        (exists u. forall v. F(u,x) ==> G(v,u) /\ G(u,x))
        ==> (exists u. forall v. F(u,y) ==> G(v,u) /\ G(u,y)) \/
            (forall u v. exists w. G(v,u) \/ H(w,y,u) ==> G(u,w))>>;;

let gilmore_7 = time resolution
 <<(forall x. K(x) ==> exists y. L(y) /\ (F(x,y) ==> G(x,y))) /\
   (exists z. K(z) /\ forall u. L(u) ==> F(z,u))
   ==> exists v w. K(v) /\ L(w) /\ G(v,w)>>;;

let gilmore_8 = time resolution
 <<exists x. forall y z.
        ((F(y,z) ==> (G(y) ==> (forall u. exists v. H(u,v,x)))) ==> F(x,x)) /\
        ((F(z,x) ==> G(x)) ==> (forall u. exists v. H(u,v,z))) /\
        F(x,y)
        ==> F(z,z)>>;;

(*** This one still isn't easy!

let gilmore_9 = time resolution
 <<forall x. exists y. forall z.
        ((forall u. exists v. F(y,u,v) /\ G(y,u) /\ ~H(y,x))
          ==> (forall u. exists v. F(x,u,v) /\ G(z,u) /\ ~H(x,z))
             ==> (forall u. exists v. F(x,u,v) /\ G(y,u) /\ ~H(x,y))) /\
        ((forall u. exists v. F(x,u,v) /\ G(y,u) /\ ~H(x,y))
         ==> ~(forall u. exists v. F(x,u,v) /\ G(z,u) /\ ~H(x,z))
             ==> (forall u. exists v. F(y,u,v) /\ G(y,u) /\ ~H(y,x)) /\
                 (forall u. exists v. F(z,u,v) /\ G(y,u) /\ ~H(z,y)))>>;;

 ***)

(* ------------------------------------------------------------------------- *)
(* Example from Davis-Putnam papers where Gilmore procedure is poor.         *)
(* ------------------------------------------------------------------------- *)

let davis_putnam_example = time resolution
 <<exists x. exists y. forall z.
        (F(x,y) ==> (F(y,z) /\ F(z,z))) /\
        ((F(x,y) /\ G(x,y)) ==> (G(x,z) /\ G(z,z)))>>;;

(* ------------------------------------------------------------------------- *)
(* The (in)famous Los problem.                                               *)
(* ------------------------------------------------------------------------- *)

let los = time resolution
 <<(forall x y z. P(x,y) ==> P(y,z) ==> P(x,z)) /\
   (forall x y z. Q(x,y) ==> Q(y,z) ==> Q(x,z)) /\
   (forall x y. Q(x,y) ==> Q(y,x)) /\
   (forall x y. P(x,y) \/ Q(x,y))
   ==> (forall x y. P(x,y)) \/ (forall x y. Q(x,y))>>;;

************* -}
END_INTERACTIVE;;
-}
-- ========================================================================= 
-- Resolution.                                                               
--                                                                           
-- Copyright (c) 2003-2007, John Harrison. (See "LICENSE.txt" for details.)  
-- ========================================================================= 

-- ------------------------------------------------------------------------- 
-- MGU of a set of literals.                                                 
-- ------------------------------------------------------------------------- 

mgu :: forall lit atom term v f. (Literal lit atom, Term term v f, Atom atom term v) =>
       Set.Set lit -> Map.Map v term -> Failing (Map.Map v term)
mgu l env =
    case Set.minView l of
      Just (a, rest) ->
          case Set.minView rest of
            Just (b, _) -> unify_literals env a b >>= mgu rest
            _ -> Success (solve env)
      _ -> Success (solve env)

unifiable :: (Literal lit atom, Term term v f, Atom atom term v) =>
             lit -> lit -> Bool
unifiable p q = failing (const False) (const True) (unify_literals Map.empty p q)

-- ------------------------------------------------------------------------- 
-- Rename a clause.                                                          
-- ------------------------------------------------------------------------- 

rename :: (FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
          (v -> v) -> Set.Set fof -> Set.Set fof
rename pfx cls =
    Set.map (subst (Map.fromList (zip fvs vvs))) cls
    where
      -- fvs :: [v]
      fvs = Set.toList (fv (list_disj cls))
      -- vvs :: [term]
      vvs = map (vt . pfx) fvs

-- ------------------------------------------------------------------------- 
-- General resolution rule, incorporating factoring as in Robinson's paper.  
-- ------------------------------------------------------------------------- 

resolvents :: (Literal fof atom, FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
              Set.Set fof -> Set.Set fof -> fof -> Set.Set fof -> Set.Set fof
resolvents cl1 cl2 p acc =
    if Set.null ps2 then acc else Set.fold doPair acc pairs
    where
      doPair (s1,s2) sof =
          case mgu (Set.union s1 (Set.map (.~.) s2)) Map.empty of
            Success mp -> Set.union (Set.map (subst mp) (Set.union (Set.difference cl1 s1) (Set.difference cl2 s2))) sof
            Failure _ -> sof
      -- pairs :: Set.Set (Set.Set fof, Set.Set fof)
      pairs = allpairs (,) (Set.map (Set.insert p) (allsubsets ps1)) (allnonemptysubsets ps2)
      -- ps1 :: Set.Set fof
      ps1 = Set.filter (\ q -> q /= p && unifiable p q) cl1
      -- ps2 :: Set.Set fof
      ps2 = Set.filter (unifiable ((.~.) p)) cl2

resolve_clauses :: forall fof atom v term f.
                   (Literal fof atom, FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
                   Set.Set fof -> Set.Set fof -> Set.Set fof
resolve_clauses cls1 cls2 =
    let cls1' = rename (prefix "x") cls1
        cls2' = rename (prefix "y") cls2 in
    Set.fold (resolvents cls1' cls2') Set.empty cls1'

-- ------------------------------------------------------------------------- 
-- Basic "Argonne" loop.                                                     
-- ------------------------------------------------------------------------- 

resloop1 :: forall atom v term f fof. (Literal fof atom, FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
            Set.Set (Set.Set fof) -> Set.Set (Set.Set fof) -> Failing Bool
resloop1 used unused =
    maybe (Failure ["No proof found"]) step (Set.minView unused)
    where
      step (cl, ros) =
          if Set.member Set.empty news then return True else resloop1 used' (Set.union ros news)
          where
            used' = Set.insert cl used
            -- resolve_clauses is not in the Failing monad, so setmapfilter isn't appropriate.
            news = Set.fold Set.insert Set.empty ({-setmapfilter-} Set.map (resolve_clauses cl) used')

pure_resolution1 :: forall fof atom v term f. (Literal fof atom, FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
                    fof -> Failing Bool
pure_resolution1 fm = resloop1 Set.empty (simpcnf (specialize (pnf fm)))

resolution1 :: forall m fof term f atom v.
               (Literal fof atom,
                FirstOrderFormula fof atom v,
                PropositionalFormula fof atom,
                Term term v f,
                Atom atom term v,
                Ord fof,
                Monad m) =>
               fof -> SkolemT v term m (Set.Set (Failing Bool))
resolution1 fm = askolemize ((.~.)(generalize fm)) >>= return . Set.map (pure_resolution1 . list_conj) . (simpdnf :: fof -> Set.Set (Set.Set fof))

-- ------------------------------------------------------------------------- 
-- Matching of terms and literals.                                           
-- ------------------------------------------------------------------------- 

term_match :: forall term v f. (Term term v f) => Map.Map v term -> [(term, term)] -> Failing (Map.Map v term)
term_match env [] = Success env
term_match env ((p, q) : oth) =
    foldTerm v fn p
    where
      v x = if not (defined env x)
            then term_match ((x |-> q) env) oth
            else if apply env x == Just q
                 then term_match env oth
                 else Failure ["term_match"]
      fn f fa =
          foldTerm v' fn' q
          where
            fn' g ga | f == g && length fa == length ga = term_match env (zip fa ga ++ oth)
            fn' _ _ = Failure ["term_match"]
            v' _ = Failure ["term_match"]
{-
  case eqs of
    [] -> Success env
    (Fn f fa, Fn g ga) : oth
        | f == g && length fa == length ga ->
           term_match env (zip fa ga ++ oth)
    (Var x, t) : oth ->
        if not (defined env x) then term_match ((x |-> t) env) oth
        else if apply env x == t then term_match env oth
        else Failure ["term_match"]
    _ -> Failure ["term_match"]
-}

match_literals :: forall term f v fof atom. (FirstOrderFormula fof atom v, Atom atom term v, Term term v f) =>
                  Map.Map v term -> fof -> fof -> Failing (Map.Map v term)
match_literals env t1 t2 =
    fromMaybe err (zipFirstOrder qu co tf at t1 t2)
    where
      qu _ _ _ _ _ _ = Nothing
      co ((:~:) p) ((:~:) q) = Just $ match_literals env p q
      co _ _ = Nothing
      tf a b = if a == b then Just (Success env) else Nothing
      at a1 a2 = Just (match env a1 a2)
      err = Failure ["match_literals"]

-- Identical to unifyAtomsEq except calls term_match instead of unify.
matchAtomsEq :: forall v f atom p term.
                (AtomEq atom p term, Term term v f) =>
                Map.Map v term -> atom -> atom -> Failing (Map.Map v term)
matchAtomsEq env a1 a2 =
    fromMaybe err (zipAtomsEq ap tf eq a1 a2)
    where
      ap p ts1 q ts2 =
          if p == q && length ts1 == length ts2
          then Just (term_match env (zip ts1 ts2))
          else Nothing
      tf p q = if p == q then Just (Success env) else Nothing
      eq pl pr ql qr = Just (term_match env [(pl, ql), (pr, qr)])
      err = Failure ["matchAtomsEq"]

{-
    case tmp of
      (Atom (R p a1), Atom(R q a2)) -> term_match env [(Fn p a1, Fn q a2)]
      (Not (Atom (R p a1)), Not (Atom (R q a2))) -> term_match env [(Fn p a1, Fn q a2)]
      _ -> Failure ["match_literals"]
-}

-- ------------------------------------------------------------------------- 
-- Test for subsumption                                                      
-- ------------------------------------------------------------------------- 

subsumes_clause :: forall term f v fof atom. (FirstOrderFormula fof atom v, Term term v f, Atom atom term v) =>
                   Set.Set fof -> Set.Set fof -> Bool
subsumes_clause cls1 cls2 =
    failing (const False) (const True) (subsume Map.empty cls1)
    where
      -- subsume :: Map.Map v term -> Set.Set fof -> Failing (Map.Map v term)
      subsume env cls =
          case Set.minView cls of
            Nothing -> Success env
            Just (l1, clt) -> settryfind (\ l2 -> case (match_literals env l1 l2) of
                                                    Success env' -> subsume env' clt
                                                    Failure msgs -> Failure msgs) cls2
-- ------------------------------------------------------------------------- 
-- With deletion of tautologies and bi-subsumption with "unused".            
-- ------------------------------------------------------------------------- 

replace :: forall term f v fof atom. (FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
           Set.Set fof
        -> Set.Set (Set.Set fof)
        -> Set.Set (Set.Set fof)
replace cl st =
    case Set.minView st of
      Nothing -> Set.singleton cl
      Just (c, st') -> if subsumes_clause cl c
                       then Set.insert cl st'
                       else Set.insert c (replace cl st')

incorporate :: forall fof term f v atom. (FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
               Set.Set fof
            -> Set.Set fof
            -> Set.Set (Set.Set fof)
            -> Set.Set (Set.Set fof)
incorporate gcl cl unused =
    if trivial cl || setAny (\ c -> subsumes_clause c cl) (Set.insert gcl unused)
    then unused
    else replace cl unused

resloop2 :: forall fof term f v atom. (Literal fof atom, FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
            Set.Set (Set.Set fof)
         -> Set.Set (Set.Set fof)
         -> Failing Bool
resloop2 used unused =
    case Set.minView unused of
      Nothing -> Failure ["No proof found"]
      Just (cl {- :: Set.Set fof-}, ros {- :: Set.Set (Set.Set fof) -}) ->
          -- print_string(string_of_int(length used) ^ " used; "^ string_of_int(length unused) ^ " unused.");
          -- print_newline();
          let used' = Set.insert cl used in
          let news = {-Set.fold Set.union Set.empty-} (Set.map (resolve_clauses cl) used') in
          if Set.member Set.empty news then return True else resloop2 used' (Set.fold (incorporate cl) ros news)

pure_resolution2 :: forall fof atom v term f. (Literal fof atom, FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
                    fof -> Failing Bool
pure_resolution2 fm = resloop2 Set.empty (simpcnf (specialize (pnf fm)))

resolution2 :: forall fof atom term v f m.
               (Literal fof atom, FirstOrderFormula fof atom v, PropositionalFormula fof atom, Term term v f, Atom atom term v, Ord fof, Monad m) =>
               fof -> SkolemT v term m (Set.Set (Failing Bool))
resolution2 fm = askolemize ((.~.) (generalize fm)) >>= return . Set.map (pure_resolution2 . list_conj) . (simpdnf :: fof -> Set.Set (Set.Set fof))

-- ------------------------------------------------------------------------- 
-- Positive (P1) resolution.                                                 
-- ------------------------------------------------------------------------- 

presolve_clauses :: (Literal fof atom, FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
                    Set.Set fof -> Set.Set fof -> Set.Set fof
presolve_clauses cls1 cls2 =
    if setAll positive cls1 || setAll positive cls2
    then resolve_clauses cls1 cls2
    else Set.empty

presloop :: (Literal fof atom, FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
            Set.Set (Set.Set fof) -> Set.Set (Set.Set fof) -> Failing Bool
presloop used unused =
    case Set.minView unused of
      Nothing -> Failure ["No proof found"]
      Just (cl, ros) ->
          -- print_string(string_of_int(length used) ^ " used; "^ string_of_int(length unused) ^ " unused.");
          -- print_newline();
          let used' = Set.insert cl used in
          let news = Set.map (presolve_clauses cl) used' in
          if Set.member Set.empty news
          then Success True
          else presloop used' (Set.fold (incorporate cl) ros news)

pure_presolution :: forall fof atom v term f. (Literal fof atom, FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
                    fof -> Failing Bool
pure_presolution fm = presloop Set.empty (simpcnf (specialize (pnf fm)))

presolution :: forall fof atom term v f m.
               (Literal fof atom, FirstOrderFormula fof atom v, PropositionalFormula fof atom, Term term v f, Atom atom term v, Ord fof, Monad m) =>
               fof -> SkolemT v term m (Set.Set (Failing Bool))
presolution fm =
    askolemize ((.~.) (generalize fm)) >>= return . Set.map (pure_presolution . list_conj) . (simpdnf :: fof -> Set.Set (Set.Set fof))

-- ------------------------------------------------------------------------- 
-- Introduce a set-of-support restriction.                                   
-- ------------------------------------------------------------------------- 

pure_resolution3 :: (Literal fof atom, FirstOrderFormula fof atom v, Term term v f, Atom atom term v, Ord fof) =>
                    fof -> Failing Bool
pure_resolution3 fm =
    uncurry resloop2 (Set.partition (setAny positive) (simpcnf (specialize (pnf fm))))

resolution3 :: forall fof atom term v f m. (Literal fof atom, FirstOrderFormula fof atom v, PropositionalFormula fof atom, Term term v f, Atom atom term v, Ord fof, Monad m) =>
               fof -> SkolemT v term m (Set.Set (Failing Bool))
resolution3 fm =
    askolemize ((.~.)(generalize fm)) >>= return . Set.map (pure_resolution3 . list_conj) . (simpdnf :: fof -> Set.Set (Set.Set fof))
