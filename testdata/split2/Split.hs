-- Split a module with a re-export
module Split
    ( dropWhile
    , foo
    ) where

import Data.List (dropWhile)

foo :: Int
foo = 1
