module A
    ( module B.A
    , module B.B
    , module C
    , module D
    , module E
    ) where

import B.A (a)
import B.B (b)
import C (c, d)
import D
import E
