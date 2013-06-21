-- Split a module with a re-export
module Split.Foo
    ( foo
    ) where


import Data.List (dropWhile)

-- Exported symbols.
foo :: Int
foo = 1


