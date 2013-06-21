{-# LANGUAGE NoImplicitPrelude #-}
-- Split a module with a re-export
module Split.Internal.Unexp
    ( unexp
    ) where


import Data.List (dropWhile)

-- Unexported symbols go into the Internal module
unexp :: Int
unexp = 3


