{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.OtherSymbols
    ( (+-+)
    ) where


import Data.Char (isAlphaNum)
import Data.List (dropWhile)
import URL

-- Symbols that can't be turned into module names go into OtherSymbols
(+-+) :: Int
(+-+) = 2
