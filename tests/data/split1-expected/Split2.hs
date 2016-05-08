{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Split2
    ( pprint1
    , pprintW
    , pprintL
    , pprintStyle
    , friendlyNames
    ) where

import Control.Lens hiding (cons)
import Data.Generics (Data, everywhere, mkT)
import Language.Haskell.TH (Ppr(ppr))
import Language.Haskell.TH.PprLib (ptext, to_HPJ_Doc)
import Language.Haskell.TH.Syntax (Name(Name), NameFlavour(NameS))
import qualified Text.PrettyPrint as HPJ (Mode(LeftMode, OneLineMode), renderStyle, style, Style(lineLength, mode))

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
      -- (this comment ends up in the wrong place, because
      -- it is associated with class OverTypes declaration below.)
