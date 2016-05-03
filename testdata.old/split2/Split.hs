{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split
    ( -- Symbols that are exported but not declared go into ReExported
      dropWhile
    , foo
    , (+-+)
    , clean
    ) where

import Data.Char (isAlphaNum)
import Data.List (dropWhile)
import URL

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

clean :: (ToURL url, Show (URLT url)) => url -> String
clean = filter isAlphaNum . show . toURL
