{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Text.Format where

import Text.PrettyPrint.ANSI.Leijen (Doc, text)

-- | This is a private Pretty class that doesn't have built-in instances
-- for tuples or lists or anything else.
class Pretty a where
    pretty :: a -> Doc

instance Pretty a => Pretty [a] where
  pretty = text . show .map pretty
