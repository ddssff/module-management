module M1
    ( c
    , module In1
    , module In2
    ) where

import In1 (a)
import In2 as Foo (b)

c = In1.a + Foo.b
