{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.Internal.Unexp
    ( unexp
    ) where




-- Unexported symbols go into the Internal module
unexp :: Int
unexp = 3

