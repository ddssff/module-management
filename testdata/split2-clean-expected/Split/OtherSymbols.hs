{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.OtherSymbols
    ( (+-+)
    ) where




-- Symbols that can't be turned into module names go into OtherSymbols
(+-+) :: Int
(+-+) = 2
