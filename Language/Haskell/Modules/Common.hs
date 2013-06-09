{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Common
    ( groupBy'
    , voidName
    , mapNames
    , ModuleResult(..)
    , modulePathBase
    , withCurrentDirectory
    ) where

import Control.Exception (bracket)
import Data.Default (def, Default)
import Data.List (groupBy, sortBy)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Name(..))
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..))
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

voidName :: A.Name a -> A.Name ()
voidName (A.Ident _ x) = A.Ident () x
voidName (A.Symbol _ x) = A.Symbol () x

mapNames :: Default a => [A.Name ()] -> [A.Name a]
mapNames [] = []
mapNames (A.Ident () x : more) = A.Ident def x : mapNames more
mapNames (A.Symbol () x : more) = A.Symbol def x : mapNames more

data ModuleResult
    = Unchanged S.ModuleName
    | Removed S.ModuleName
    | Modified S.ModuleName String
    deriving (Show, Eq, Ord)

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
