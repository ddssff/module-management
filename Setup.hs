#!/usr/bin/runhaskell

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
-- import Language.Haskell.Imports.Clean (cleanBuildImports)
-- import Language.Haskell.Imports.Params (runParamsT)
import System.Cmd (system)
import System.Exit (ExitCode(ExitSuccess))
import System.Directory (setCurrentDirectory)
import System.FilePath ((</>))

main = defaultMainWithHooks simpleUserHooks
         { sDistHook = \ p l u f ->
             system "tar cfz testdata.tar.gz testdata" >> (sDistHook simpleUserHooks) p l u f }
       -- {
       --   postBuild = \ _ _ _ lbi -> do code <- system (buildDir lbi </> "tests/tests")
       --                                 if code == ExitSuccess then putStrLn "Tests passed" else error "Tests failed"
       -- }
