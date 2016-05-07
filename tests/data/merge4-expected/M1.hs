module M1
    ( c
    , module Out
    , module Out
    ) where

import Out (a)
import Out as Foo (b)

c = Out.a + Foo.b
