{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.Internal.Unexp
    ( unexp
    ) where


import Data.Char (isAlphaNum)
import Data.List (dropWhile)
import URL

-- Unexported symbols go into the Internal module
unexp :: Int
unexp = 3

