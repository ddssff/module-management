{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.Internal.Unexp
    ( -- Symbols that are exported but not declared go into ReExported
      unexp
    ) where



-- Unexported symbols go into the Internal module
unexp :: Int
unexp = 3
