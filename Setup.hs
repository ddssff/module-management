#!/usr/bin/runhaskell

import Distribution.Simple
import System.Directory (copyFile)

main = copyFile "debian/changelog" "changelog" >>
       defaultMainWithHooks simpleUserHooks
