{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.Internal.Bar
    ( -- Symbols that are exported but not declared go into ReExported
      Bar(Bar)
    ) where

import Data.Char (isAlphaNum)
import Data.List (dropWhile)
import URL

newtype Bar = Bar Int
