{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.Internal.Bar
    ( Bar(Bar)
    ) where


import Data.Char (isAlphaNum)
import Data.List (dropWhile)
import URL

newtype Bar = Bar Int
