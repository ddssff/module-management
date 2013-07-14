{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.Internal.Bar
    ( -- Symbols that are exported but not declared go into ReExported
      Bar(Bar)
    ) where



newtype Bar = Bar Int
