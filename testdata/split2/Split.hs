{-# LANGUAGE NoImplicitPrelude #-}
-- Split a module with a re-export
module Split
    ( -- Symbols that are exported but not declared go into ReExported
      dropWhile
    , foo
    , (+-+)
    ) where

import Data.List (dropWhile)

-- Exported symbols.
foo :: Int
foo = 1

-- Symbols that can't be turned into module names go into OtherSymbols
(+-+) :: Int
(+-+) = 2

-- Unexported symbols go into the Internal module
unexp :: Int
unexp = 3

newtype Bar = Bar Int

instance Show Bar where
    show (Bar n) = "Bar " ++ show n
