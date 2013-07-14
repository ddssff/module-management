{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PackageImports, TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |AptIO is an instance of the RWS monad used to manage the global
-- state and output style parameters of clients of the Apt library,
-- such as the autobuilder.
module Apt.InitState
    ( initState
    ) where

import qualified Data.Map as Map (empty)
import Apt.AptState (AptState(AptState, aptImageMap, binaryPackageMap, releaseMap, repoMap, sourcePackageMap))

-- |The initial output state - at the beginning of the line, no special handle
-- state information, no repositories in the repository map.
initState :: AptState
initState = AptState
            { repoMap = Map.empty
            , releaseMap = Map.empty
            , aptImageMap = Map.empty
            , sourcePackageMap = Map.empty
            , binaryPackageMap = Map.empty
            }
