-- | Ensure that any OSKey or AptKey value available outside this
-- module corresponds to an existing entry in the corresponding
-- Map in ReposState.
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Internal.IO
    ( buildArchOfRoot
    ) where

import Debian.Arch (Arch(..), ArchCPU(..), ArchOS(..))
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)

buildArchOfRoot :: IO Arch
buildArchOfRoot =
    do a@(code1, out1, _err1) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_OS"] ""
       b@(code2, out2, _err2) <- readProcessWithExitCode "dpkg-architecture" ["-qDEB_BUILD_ARCH_CPU"] ""
       case (code1, lines out1, code2, lines out2) of
         (ExitSuccess, os : _, ExitSuccess, cpu : _) ->
             return $ Binary (parseArchOS os) (parseArchCPU cpu)
         _ -> error $ "Failure computing build architecture of /: " ++ show (a, b)
    where
      parseArchOS "any" = ArchOSAny
      parseArchOS x = ArchOS x
      parseArchCPU "any" = ArchCPUAny
      parseArchCPU x = ArchCPU x
