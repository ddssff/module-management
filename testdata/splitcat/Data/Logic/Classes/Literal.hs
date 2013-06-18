{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal
    ( fixityLiteral, foldAtomsLiteral, Literal(foldLiteral), prettyLit, toPropositional, zipLiterals
    ) where

import Data.Logic.Classes.Constants (Constants, Constants(fromBool))
import Data.Logic.Classes.Formula (Formula, Formula(atomic))
import Data.Logic.Classes.Negate ((.~.), Negatable, negated)
import Data.Logic.Classes.Pretty (Fixity(..), FixityDirection(..), HasFixity(..))
import qualified Data.Logic.Classes.Propositional as P (PropositionalFormula)
import Text.PrettyPrint ((<>), Doc, nest, parens, text)

class (Negatable lit, Constants lit, HasFixity atom, Formula lit atom, Ord lit) => Literal lit atom | lit -> atom where
    foldLiteral :: (lit -> r) -> (Bool -> r) -> (atom -> r) -> lit -> r

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

toPropositional :: forall lit atom pf atom2. (Literal lit atom, P.PropositionalFormula pf atom2) =>
                   (atom -> atom2) -> lit -> pf
toPropositional ca lit = foldLiteral (\ p -> (.~.) (toPropositional ca p)) fromBool (atomic . ca) lit

{-
prettyLit :: forall lit atom term v p f. (Literal lit atom v, Apply atom p term, Term term v f) =>
              (v -> Doc)
           -> (p -> Doc)
           -> (f -> Doc)
           -> Int
           -> lit
           -> Doc
prettyLit pv pp pf _prec lit =
    foldLiteral neg tf at lit
    where
      neg :: lit -> Doc
      neg x = if negated x then text {-"¬"-} "~" <> prettyLit pv pp pf 5 x else prettyLit pv pp pf 5 x
      tf = text . ifElse "true" "false"
      at = foldApply (\ pr ts -> 
                        pp pr <> case ts of
                                   [] -> empty
                                   _ -> parens (hcat (intersperse (text ",") (map (prettyTerm pv pf) ts))))
                   (\ x -> text $ if x then "true" else "false")
      -- parensIf False = id
      -- parensIf _ = parens . nest 1
-}

prettyLit :: forall lit atom v. (Literal lit atom) =>
              (Int -> atom -> Doc)
           -> (v -> Doc)
           -> Int
           -> lit
           -> Doc
prettyLit pa pv pprec lit =
    parensIf (pprec > prec) $ foldLiteral co tf at lit
    where
      co :: lit -> Doc
      co x = if negated x then text {-"¬"-} "~" <> prettyLit pa pv 5 x else prettyLit pa pv 5 x
      tf x = text (if x then "true" else "false")
      at = pa 6
      parensIf False = id
      parensIf _ = parens . nest 1
      Fixity prec _ = fixityLiteral lit

fixityLiteral :: (Literal formula atom) => formula -> Fixity
fixityLiteral formula =
    foldLiteral neg tf at formula
    where
      neg _ = Fixity 5 InfixN
      tf _ = Fixity 10 InfixN
      at = fixity

foldAtomsLiteral :: Literal lit atom => (r -> atom -> r) -> r -> lit -> r
foldAtomsLiteral f i lit = foldLiteral (foldAtomsLiteral f i) (const i) (f i) lit
