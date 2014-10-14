module Debian.Repo.Prelude.Misc
    (
    -- * String functions
      columns
    , justify
    -- * Tuple functions
    , mapSnd
    -- * FilePath functions
    , parentPath
    , canon
    -- * Map and Set functions
    , listMap
    , listDiff
    -- * System.Posix
    , checkSuperUser
    , md5sum
    , sameInode
    , sameMd5sum
    , tarDir
    -- * Processes
    -- , splitOutput
    -- * ByteString
    , cd
    -- * Debugging
    , read'
    ) where

import		 Control.Exception
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Digest.Pure.MD5
import		 Data.List
import qualified Data.Map as Map
import		 Data.Maybe
import qualified Data.Set as Set
import		 Debian.Repo.Prelude.List
import           System.Exit
import		 System.FilePath
import		 System.Directory
import		 System.Posix.Files
import		 System.Posix.User (getEffectiveUserID)
import           System.Process (readProcessWithExitCode)
-- import System.Process.Progress (keepStdout, keepStderr, keepResult)
import		 Text.Regex

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

-- Control file stuff

-- |Pad strings so the columns line up. The argument and return value
-- elements are the rows of a table.  Do not pad the rightmost column.
columns :: [[String]] -> [[String]]
columns rows =
    map (map pad . zip (widths ++ [0])) rows
    where
      widths = map (fromJust . listMax) . transpose . map (map length . init) $ rows
      listMax l = foldl (\ a b -> Just . maybe b (max b) $ a) Nothing l
      pad (width, field) = field ++ replicate (max 0 (width - length field)) ' '

-- |Group words into lines of length n or less.
justify :: String -> Int -> [[String]]
justify s n =
    foldr doWord [[]] (words s)
    where doWord w [] = [[w]]
	  doWord w (ws : etc) = 
	      if length (concat (intersperse " " (w:ws))) <= n then
		 (w : ws) : etc else
		 [w] : ws : etc

-- |dirname
parentPath :: FilePath -> FilePath
parentPath path = fst (splitFileName path)

-- |Turn a list of (k, a) pairs into a map from k -> [a].  The order of the elements in
-- the a list is preserved.
listMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
listMap pairs =
    foldl insertPair Map.empty (reverse pairs)
    where insertPair m (k,a) = Map.insert k (a : (Map.findWithDefault [] k m)) m

-- Return the difference of two lists.  Order is not preserved.
listDiff :: Ord a => [a] -> [a] -> [a]
listDiff a b = Set.toList (Set.difference (Set.fromList a) (Set.fromList b))

-- | Weak attempt at canonicalizing a file path.
canon :: FilePath -> FilePath
canon path =
    let re = mkRegex "/" in
    let names = splitRegex re path in
    concat (intersperse "/" (merge names))
    where
      merge (".." : xs) = ".." : (merge xs)
      merge ("." : xs) = "." : (merge xs)
      merge (_ : ".." : xs) = (merge xs)
      merge (x : "." : xs) = (merge (x : xs))
      merge (x : xs) = x : merge xs
      merge [] = []

{-# DEPRECATED md5sum "Use Data.ByteString.Lazy.Char8.readFile path >>= return . show . Data.Digest.Pure.MD5.md5" #-}
-- | Run md5sum on a file and return the resulting checksum as text.
md5sum :: FilePath -> IO String
md5sum path = B.readFile path >>= return . show . Data.Digest.Pure.MD5.md5

-- | Predicate to decide if two files have the same inode.
sameInode :: FilePath -> FilePath -> IO Bool
sameInode a b =
    do
      aStatus <- getFileStatus a
      bStatus <- getFileStatus b
      return (deviceID aStatus == deviceID bStatus && fileID aStatus == fileID bStatus)

-- | Predicate to decide if two files have the same md5 checksum.
sameMd5sum :: FilePath -> FilePath -> IO Bool
sameMd5sum a b =
    do
      asum <- md5sum a
      bsum <- md5sum b
      return (asum == bsum)

{-
splitOutput :: [Output B.ByteString] -> (B.ByteString, B.ByteString, [ExitCode])
splitOutput output = (B.concat (keepStdout output), B.concat (keepStderr output), keepResult output)
-}

-- |A version of read with a more helpful error message.
read' :: Read a => String -> a
read' s =
    case reads s of
      [] -> error $ "read - no parse: " ++ show s
      ((x, _s) : _) -> x

checkSuperUser :: IO Bool
checkSuperUser = getEffectiveUserID >>= return . (== 0)

-- | Given a tarball, return the name of the top directory.
tarDir :: FilePath -> IO (Maybe String)
tarDir path =
    readProcessWithExitCode "tar" ["tfz", path] "" >>= \ (code, out, _) ->
    case code of
      ExitSuccess -> return . dir . lines $ out
      _ -> return Nothing
    where
      dir [] = Nothing
      dir (file : _) = case wordsBy (== '/') file of
                         [] -> Nothing
                         ("" : _) -> Nothing
                         (s : _) -> Just s

cd :: FilePath -> IO a -> IO a
cd name m = 
    bracket 
        (do cwd <- getCurrentDirectory
            setCurrentDirectory name
            return cwd)
        (\oldwd -> do setCurrentDirectory oldwd)
        (const m)
