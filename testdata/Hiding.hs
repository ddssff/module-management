{-# LANGUAGE OverloadedStrings #-}
module Hiding where

import Prelude hiding (writeFile, unlines)
import Data.Text (unlines)
import Data.Text.IO (writeFile)

main :: IO ()
main = writeFile "/tmp/foo" (unlines ["hello", "world"])
