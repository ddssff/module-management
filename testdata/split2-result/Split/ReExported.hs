{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.ReExported
    ( -- Symbols that are exported but not declared go into ReExported
      dropWhile
    ) where

import Data.Char (isAlphaNum)
import Data.List (dropWhile)
import URL
