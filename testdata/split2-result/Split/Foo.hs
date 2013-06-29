{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.Foo
    ( foo
    ) where


import Data.Char (isAlphaNum)
import Data.List (dropWhile)
import URL

-- Exported symbols.
foo :: Int
foo = 1
