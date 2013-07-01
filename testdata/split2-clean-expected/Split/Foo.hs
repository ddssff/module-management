{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.Foo
    ( -- Symbols that are exported but not declared go into ReExported
      foo
    ) where




-- Exported symbols.
foo :: Int
foo = 1
