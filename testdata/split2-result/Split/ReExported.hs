-- Split a module with a re-export
module Split.ReExported
    ( -- Symbols that are exported but not declared go into ReExported
      dropWhile
    ) where

-- Due to http://hackage.haskell.org/trac/ghc/ticket/8000 this import gets deleted.
import Data.List (dropWhile)

