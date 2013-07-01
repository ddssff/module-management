{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.OtherSymbols
    ( -- Symbols that are exported but not declared go into ReExported
      (+-+)
    ) where




-- Symbols that can't be turned into module names go into OtherSymbols
(+-+) :: Int
(+-+) = 2
