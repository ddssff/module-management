{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Imports.Common
    ( Display(..)
    , HasSrcLoc(..)
    , HasSrcSpan(..)
    -- , importsSpan
    -- , renameSpec
    -- , specName
    -- , replaceImports
    , tildeBackup
    , noBackup
    , replaceFile
    , removeFile'
    , groupBy'
    , withCurrentDirectory
    , untabify
    , lines'
    ) where

import Control.Exception (catch, throw, bracket)
import Control.Monad.Trans (liftIO)
import Data.Default (def, Default)
import Data.List (groupBy, sortBy)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcSpan(SrcSpan))
import Language.Haskell.Exts.Syntax (Decl, ImportDecl, Module(..), SrcLoc(SrcLoc))
import System.Directory (getCurrentDirectory, removeFile, renameFile, setCurrentDirectory)
import System.IO.Error (isDoesNotExistError)

tildeBackup :: FilePath -> Maybe FilePath
tildeBackup = Just . (++ "~")

noBackup :: FilePath -> Maybe FilePath
noBackup = const Nothing

-- | Replace the file at path with the given text, moving the original
-- to the location returned by passing path to backup.  If backup is
-- the identity function you're going to have a bad time.
replaceFile :: Bool -> (FilePath -> Maybe FilePath) -> FilePath -> String -> IO ()
replaceFile dryRun' backup path text =
    case dryRun' of
      True -> liftIO $ putStrLn ("dryRun: replaceFile " ++ show path ++ " " ++ show text)
      False -> liftIO $ remove >> rename >> write
    where
      remove = maybe (return ()) removeFile (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      rename = maybe (return ()) (renameFile path) (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      write = writeFile path text

removeFile' :: FilePath -> IO ()
removeFile' path = putStrLn ("removeFile " ++ show path) >> removeFile path

class Display a where
    display :: a -> String

class HasSrcSpan a where
    srcSpan' :: a -> SrcSpan

instance Display Comment where
    display (Comment _ sp _) = "Comment " ++ display sp

instance Display ImportDecl where
    display _ = "ImportDecl"

instance Display Decl where
    display _ = "Decl"

instance Display Module where
    display _ = "Module"

instance Display SrcSpan where
    display (SrcSpan _ bl bc el ec) = "[" ++ show bl ++ ":" ++ show bc ++ " - " ++ show el ++ ":" ++ show ec ++ "]"

instance Display SrcLoc where
    display (SrcLoc _ b e) = "[" ++ show b ++ ":" ++ show e ++ "]"

instance Default SrcLoc where
    def = SrcLoc "<unknown>.hs" 1 1

-- | Class of values that contain a source location.
class HasSrcLoc x where
    srcLoc :: x -> SrcLoc

instance HasSrcLoc SrcSpan where
    srcLoc (SrcSpan f b e _ _) = SrcLoc f b e

instance HasSrcLoc Module where
    srcLoc (Module s _ _ _ _ _ _) = s

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path action =
    bracket (getCurrentDirectory >>= \ save -> setCurrentDirectory path >> return save)
            setCurrentDirectory
            (const action)

untabify :: String -> String
untabify s =
    loop 0 s
    where
      loop :: Int -> String -> String
      loop n ('\t' : s') = replicate (8 - mod n 8) ' ' ++ loop 0 s'
      loop _ ('\n' : s') = '\n' : loop 0 s'
      loop n (c : s') = c : loop (n + 1) s'
      loop _ [] = []

-- | Convert a compare function into an (==)
toEq :: Ord a => (a -> a -> Ordering) -> (a -> a -> Bool)
toEq cmp a b =
    case cmp a b of
      EQ -> True
      _ -> False

-- | Combine sortBy and groupBy
groupBy' :: Ord a => (a -> a -> Ordering) -> [a] -> [[a]]
groupBy' cmp xs = groupBy (toEq cmp) $ sortBy cmp xs

-- | A version of lines that preserves the presence or absence of a
-- terminating newline
lines' :: String -> [String]
lines' s =
    bol (groupBy (\ a b -> a /= '\n' && b /= '\n') s)
    where
      -- If we are at beginning of line and see a newline, insert an empty
      bol ("\n" : xs) = "" : bol xs
      -- If we are at beginning of line and see something else, call end of line
      bol (x : xs) = x : eol xs
      -- If we see EOF at bol insert a trailing empty
      bol [] = [""]
      -- If we are at end of line and see a newline, go to beginning of line
      eol ("\n" : xs) = bol xs
      -- This shouldn't happen
      eol (x : xs) = x : eol xs
      eol [] = []
