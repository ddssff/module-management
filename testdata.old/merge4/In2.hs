module In2
    ( b
    , module In1
    ) where

import In1 as Foo

b = 2 + Foo.a
