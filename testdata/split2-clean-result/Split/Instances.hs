{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.Instances
    ( 
    ) where

import Split.Internal.Bar (Bar(Bar))

instance Show Bar where
    show (Bar n) = "Bar " ++ show n


