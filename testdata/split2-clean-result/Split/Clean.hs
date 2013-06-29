{-# LANGUAGE FlexibleContexts #-}
-- Split a module with a re-export
module Split.Clean
    ( clean
    ) where


import Data.Char (isAlphaNum)
import URL (ToURL(toURL), URLT)

clean :: (ToURL url, Show (URLT url)) => url -> String
clean = filter isAlphaNum . show . toURL
