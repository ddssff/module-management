{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.Instances
    ( -- Symbols that are exported but not declared go into ReExported
      
    ) where

import Split.Internal.Bar (Bar(Bar))

import Data.Char (isAlphaNum)
import Data.List (dropWhile)
import URL

instance Show Bar where
    show (Bar n) = "Bar " ++ show n
