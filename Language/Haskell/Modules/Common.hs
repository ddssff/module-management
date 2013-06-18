{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Common
    ( groupBy'
    , mapNames
    , modulePathBase
    , withCurrentDirectory
    ) where

import Control.Exception (bracket)
import Data.Default (def, Default)
import Data.List (groupBy, sortBy)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Name(..))
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..), Name(..))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((<.>))

-- | Convert a compare function into an (==)
toEq :: Ord a => (a -> a -> Ordering) -> (a -> a -> Bool)
toEq cmp a b =
    case cmp a b of
      EQ -> True
      _ -> False

-- | Combine sortBy and groupBy
groupBy' :: Ord a => (a -> a -> Ordering) -> [a] -> [[a]]
groupBy' cmp xs = groupBy (toEq cmp) $ sortBy cmp xs

mapNames :: Default a => [S.Name] -> [A.Name a]
mapNames [] = []
mapNames (S.Ident x : more) = A.Ident def x : mapNames more
mapNames (S.Symbol x : more) = A.Symbol def x : mapNames more

-- | Construct the base of a module path.
modulePathBase :: S.ModuleName -> FilePath
modulePathBase (S.ModuleName name) =
    map f name <.> "hs"
    where
      f '.' = '/'
      f c = c

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path action =
    bracket (getCurrentDirectory >>= \ save -> setCurrentDirectory path >> return save)
            setCurrentDirectory
            (const action)
