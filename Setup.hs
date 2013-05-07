#!/usr/bin/runhaskell

import Distribution.PackageDescription (PackageDescription (executables), Executable(modulePath))
import Distribution.Simple (postConf, defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import Language.Haskell.Imports.Clean (cleanDumpedImports)

main = defaultMainWithHooks simpleUserHooks {
         postConf = \ _ _ _ lbi -> cleanDumpedImports lbi
       }
