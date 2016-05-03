{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Split1
    ( pprint1
    , pprintW
    , pprintL
    , friendlyNames
    ) where

import Control.Lens hiding (cons)
import Control.Monad (foldM)
import Data.Generics (Data, everywhere, mkT)
import Data.Graph as Graph
import Data.Map as Map (Map, fromList, toList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set as Set (fromList, Set, toList)
import Language.Haskell.TH
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax (Lift(lift), Name(Name), NameFlavour(NameS), Quasi(qReify), StrictType, VarStrictType)
import qualified Text.PrettyPrint as HPJ

instance Ppr () where
    ppr () = ptext "()"

-- | Pretty print a 'Ppr' value on a single line with each block of
-- white space (newlines, tabs, etc.) converted to a single space, and
-- all the module qualifiers removed from the names.  (If the data type
-- has no 'Name' values the friendlyNames function has no effect.)
pprint1 :: (Ppr a, Data a) => a -> [Char]
pprint1 = pprintStyle (HPJ.style {HPJ.mode = HPJ.OneLineMode}) . friendlyNames

-- | Pretty print with friendly names and wide lines
pprintW :: (Ppr a, Data a) => Int -> a -> [Char]
pprintW w = pprintStyle (HPJ.style {HPJ.lineLength = w}) . friendlyNames

-- | Pretty print with friendly names in left mode
pprintL :: (Ppr a, Data a) => a -> [Char]
pprintL = pprintStyle (HPJ.style {HPJ.mode = HPJ.LeftMode}) . friendlyNames

-- | Helper function for pprint1 et. al.
pprintStyle :: (Ppr a, Data a) => HPJ.Style -> a -> String
pprintStyle style = HPJ.renderStyle style . to_HPJ_Doc . ppr . friendlyNames

-- | Make a template haskell value more human reader friendly.  The
-- result almost certainly won't be compilable.  That's ok, though,
-- because the input is usually uncompilable - it imports hidden modules,
-- uses infix operators in invalid positions, puts module qualifiers in
-- places where they are not allowed, and maybe other things.
friendlyNames :: Data a => a -> a
friendlyNames =
    everywhere (mkT friendlyName)
    where
      friendlyName (Name x _) = Name x NameS -- Remove all module qualifiers
