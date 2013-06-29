{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS -Wwarn #-}
module Data.Logic.Classes.Literal.ZipLiterals
    ( zipLiterals
    ) where

import Data.Logic.Classes.Literal.Literal (Literal(foldLiteral))

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

