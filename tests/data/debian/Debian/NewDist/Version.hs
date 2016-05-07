{-# LANGUAGE CPP, TemplateHaskell #-}
module Version (myVersion) where

import Data.Version (showVersion)
import Paths_debian_repo (version)

myVersion :: String
myVersion = showVersion version
