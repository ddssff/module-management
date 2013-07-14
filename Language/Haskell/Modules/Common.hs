{-# LANGUAGE BangPatterns, FlexibleInstances, PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Common
    ( groupBy'
    , withCurrentDirectory
    ) where

import "MonadCatchIO-mtl" Control.Monad.CatchIO (bracket, MonadCatchIO)
import Control.Monad.Trans (liftIO)
import Data.List (groupBy, sortBy)
import System.Directory (getCurrentDirectory, setCurrentDirectory)

-- | Convert a compare function into an (==)
toEq :: Ord a => (a -> a -> Ordering) -> (a -> a -> Bool)
toEq cmp a b =
    case cmp a b of
      EQ -> True
      _ -> False

-- | Combine sortBy and groupBy
groupBy' :: Ord a => (a -> a -> Ordering) -> [a] -> [[a]]
groupBy' cmp xs = groupBy (toEq cmp) $ sortBy cmp xs

withCurrentDirectory :: MonadCatchIO m => FilePath -> m a -> m a
withCurrentDirectory path action =
    bracket (liftIO getCurrentDirectory >>= \ save -> liftIO (setCurrentDirectory path) >> return save)
            (liftIO . setCurrentDirectory)
            (const action)
