{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Data.Logic.Types.Harrison.Equal where

instance C.Formula (Formula FOLEQ) FOLEQ => P.PropositionalFormula (Formula FOLEQ) FOLEQ where
    foldPropositional co tf at fm =
        case fm of
          F -> tf False
          T -> tf True