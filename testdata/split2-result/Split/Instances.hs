-- Split a module with a re-export
module Split.Instances
    ( 
    ) where

import Split.Internal.Bar (Bar(Bar))

import Data.List (dropWhile)

instance Show Bar where
    show (Bar n) = "Bar " ++ show n

