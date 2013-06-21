{-# LANGUAGE NoImplicitPrelude #-}
-- Split a module with a re-export
module Split.Internal.Bar
    ( Bar(Bar)
    ) where


import Data.List (dropWhile)

newtype Bar = Bar Int


