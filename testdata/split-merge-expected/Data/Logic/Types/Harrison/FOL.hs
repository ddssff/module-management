{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables,
             TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Data.Logic.Types.Harrison.FOL
    ( TermType(..)
    , FOL(..)
    , Function(..)
    ) where

import Data.Generics (Data, Typeable)
import Data.List (intersperse)
import Data.Logic.Classes.Apply (Apply(..), Predicate)
import Data.Logic.Classes.Arity (Arity(..))
import Data.Logic.Classes.Constants (asBool, Constants(fromBool))
import Data.Logic.Classes.Pretty (Fixity(..), FixityDirection(..), HasFixity(..), Pretty(pretty))
import Data.Logic.Classes.Skolem (Skolem(..))
import Data.Logic.Classes.Term (Term(vt, foldTerm, fApp))
import qualified Data.Logic.Classes.Term as C (Function, Term(zipTerms))
import qualified Data.Logic.Types.Common ()
import Prelude hiding (pred)
import Text.PrettyPrint (cat, text)

-- -------------------------------------------------------------------------
-- Terms.                                                                   
-- -------------------------------------------------------------------------

data TermType
    = Var String
    | Fn Function [TermType]
    deriving (Eq, Ord)

data FOL = R String [TermType] deriving (Eq, Ord, Show)

instance Show TermType where
    show (Var v) = "vt " ++ show v
    show (Fn f ts) = "fApp " ++ show f ++ " " ++ show ts

instance Pretty TermType where
    pretty (Var v) = pretty v
    pretty (Fn f ts) = cat ([pretty f, text "("] ++ intersperse (text ", ") (map pretty ts) ++ [text ")"])

instance Apply FOL String TermType where
    foldApply f tf (R p ts) = maybe (f p ts) tf (asBool p)
    apply' = R

-- | This is probably dangerous.
instance Constants String where
    fromBool True = "true"
    fromBool False = "false"
    asBool x 
        | x == fromBool True = Just True
        | x == fromBool False = Just False
        | True = Nothing

instance Constants FOL where
    fromBool x = R (fromBool x) []
    asBool (R p _) = asBool p

instance Predicate String

{-
instance Pretty String where
    pretty = text

instance FirstOrderFormula (Formula FOL) FOL String where
    -- type C.Term (Formula FOL) = Term
    -- type V (Formula FOL) = String
    -- type Pr (Formula FOL) = String
    -- type Fn (Formula FOL) = String -- ^ Atomic function type

    -- quant C.Exists v fm = H.Exists v fm
    -- quant C.Forall v fm = H.Forall v fm
    for_all = H.Forall
    exists = H.Exists
    atomic = Atom
    foldFirstOrder qu co tf at fm =
        case fm of
          F -> tf False
          T -> tf True
          Atom atom -> at atom
          Not fm' -> co ((:~:) fm')
          And fm1 fm2 -> co (BinOp fm1 (:&:) fm2)
          Or fm1 fm2 -> co (BinOp fm1 (:|:) fm2)
          Imp fm1 fm2 -> co (BinOp fm1 (:=>:) fm2)
          Iff fm1 fm2 -> co (BinOp fm1 (:<=>:) fm2)
          H.Forall v fm' -> qu C.Forall v fm'
          H.Exists v fm' -> qu C.Exists v fm'
-}

instance Pretty FOL where
    pretty (R p ts) = cat ([pretty p, text "("] ++ intersperse (text ", ") (map pretty ts) ++ [text ")"])

instance Arity String where
    arity _ = Nothing

-- | The Harrison book uses String for atomic function, but we need
-- something a little more type safe because of our Skolem class.
data Function
    = FName String
    | Skolem String
    deriving (Eq, Ord, Data, Typeable, Show)

instance Pretty Function where
    pretty (FName s) = text s
    pretty (Skolem v) = text ("sK" ++ v)

instance C.Function Function String

instance Skolem Function String where
    toSkolem = Skolem
    isSkolem (Skolem _) = True
    isSkolem _ = False

instance Term TermType String Function where
    -- type V Term = String
    -- type Fn Term = String
    vt = Var
    fApp = Fn
    foldTerm vfn _ (Var x) = vfn x
    foldTerm _ ffn (Fn f ts) = ffn f ts
    zipTerms = undefined

instance HasFixity FOL where
    fixity = const (Fixity 10 InfixN)
