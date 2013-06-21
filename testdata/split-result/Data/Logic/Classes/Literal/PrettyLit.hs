{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal.PrettyLit
    ( prettyLit
    ) where

import Data.Logic.Classes.Literal.Internal.FixityLiteral (fixityLiteral)
import Data.Logic.Classes.Literal.Literal (Literal(foldLiteral))
import Data.Logic.Classes.Negate (negated)
import Data.Logic.Classes.Pretty (Fixity(Fixity))
import Text.PrettyPrint ((<>), Doc, nest, parens, text)

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


