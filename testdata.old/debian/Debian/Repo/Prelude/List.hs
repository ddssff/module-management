module Debian.Repo.Prelude.List
    ( consperse
    , surround
    , changePrefix
    , dropPrefix
    , cartesianProduct
    , wordsBy
    , empty
    , sortByMapped
    , sortByMappedM
    , partitionM
    , listIntersection
    , isSublistOf
    ) where

import Control.Monad
import Data.List

{-# DEPRECATED consperse "Use intercalate" #-}
consperse :: [a] -> [[a]] -> [a]
-- ^ The mighty consperse function - e.g. consperse "," ["a", "b"] -> "a,b"
-- consperse = MissingH.List.join
consperse s l = concat . intersperse s $ l

surround :: [a] -> [a] -> [[a]] -> [a]
-- ^ surround each element of a list - e.g. surround "(" ")" ["a", "b"] -> ["(a)(b)"]
surround prefix suffix items = concat $ map ((prefix ++) . (++ suffix)) items

-- |Replace the prefix of s, return Nothing if it doesn't match.
changePrefix :: (Eq a) => [a] -> [a] -> [a] -> Maybe [a]
changePrefix old new s = maybe Nothing (Just . (new ++)) (dropPrefix old s)

-- |Remove a prefix of s, return nothing if it doesn't match.
dropPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
dropPrefix prefix s =
    case isPrefixOf prefix s of
      True -> Just (drop (length prefix) s)
      False -> Nothing

cartesianProduct :: [[a]] -> [[a]]
-- ^ cartesianProduct [[1,2,3], [4,5],[6]] -> [[1,4,6],[1,5,6],[2,4,6],[2,5,6],[3,4,6],[3,5,6]]
cartesianProduct [] = []
cartesianProduct [xs] = map (: []) xs
cartesianProduct (xs : yss) =
    distribute xs (cartesianProduct yss)
    where distribute xs' yss' = concat (map (\ x -> map (x :) yss') xs')

-- |FIXME: implement for a string
wordsBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
wordsBy p s = 
    case (break p s) of
      (s', []) -> [s']
      (h, t) -> h : wordsBy p (drop 1 t)

-- |Like maybe, but with empty vs. non-empty list
empty :: b -> ([a] -> b) -> [a] -> b
empty e _ [] = e
empty _ f l = f l

-- |Sort a list using the compare function on the list elements mapped
-- over f.  This is like "sortBy (\ a b -> compare (f a) (f b))"
-- except that f is applied O(n) times instead of O(n log n)
sortByMapped :: (a -> b) -> (b -> b -> Ordering) -> [a] -> [a]
sortByMapped f cmp list =
    map fst sorted
    where
      sorted = sortBy (\ (_, x) (_, y) -> cmp x y) pairs
      pairs = zip list (map f list)

-- |Monadic version of sortByMapped
sortByMappedM :: (a -> IO b) -> (b -> b -> Ordering) -> [a] -> IO [a]
sortByMappedM f cmp list =
    do
      pairs <- mapM f list >>= return . (zip list)
      let sorted = sortBy (\ (_, x) (_, y) -> cmp x y) pairs
      return (map fst sorted)

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs =
    foldM f ([], []) xs
    where f (a, b) x = p x >>= (\ flag -> return $ if flag then (x : a, b) else (a, x : b))

listIntersection :: Eq a => [[a]] -> [a]
listIntersection [] = []
listIntersection (first : rest) = foldr intersect first rest

isSublistOf :: Eq a => [a] -> [a] -> Maybe Int
isSublistOf sub lst =
    maybe Nothing (\ s -> Just (length s - length sub))
              (find (isSuffixOf sub) (inits lst))

{-
lookups :: (Eq a) => a -> [(a, b)] -> [b]
lookups a = map snd . filter ((a ==) . fst)
-}
