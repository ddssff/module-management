module Debian.Repo.Prelude.Bool
    ( cond
    ) where

cond :: a -> a -> Bool -> a
cond t _ True = t
cond _ f False = f
