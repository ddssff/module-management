{-# LANGUAGE NoImplicitPrelude #-}
-- Split a module with a re-export
module Split.OtherSymbols
    ( (+-+)
    ) where


import Data.List (dropWhile)

-- Symbols that can't be turned into module names go into OtherSymbols
(+-+) :: Int
(+-+) = 2


