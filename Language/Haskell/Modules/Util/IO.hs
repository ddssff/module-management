{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Util.IO
    ( tildeBackup
    , noBackup
    , readFileMaybe
    , replaceFile
    , replaceFileIfDifferent
    , removeFileIfPresent
    , withCurrentDirectory
    ) where

import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(..), DeclHead(..), ExportSpec(..), ExportSpecList(..), ImportDecl(ImportDecl), ImportSpec(..), ImportSpecList, InstHead(..), Match(..), Module, ModuleHead(..), ModuleName(..), ModulePragma(..), Name(..), QName(..), WarningText(..), Type(..))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Control.Applicative ((<$>))
import Control.Exception (bracket, catch, throw)
import Data.Default (def, Default)
import Data.List (groupBy, intercalate, sortBy)
import Language.Haskell.Exts (ParseResult(ParseOk, ParseFailed))
import Language.Haskell.Exts.SrcLoc (srcSpanEnd, srcSpanStart)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..))
import System.Directory (getCurrentDirectory, removeFile, renameFile, setCurrentDirectory)
import System.FilePath ((<.>))
import System.IO.Error (isDoesNotExistError)

type Module = A.Module SrcSpanInfo
type ModuleHead = A.ModuleHead SrcSpanInfo
type ModulePragma = A.ModulePragma SrcSpanInfo
type ModuleName = A.ModuleName SrcSpanInfo
type WarningText = A.WarningText SrcSpanInfo
type ExportSpecList = A.ExportSpecList SrcSpanInfo
type ExportSpec = A.ExportSpec SrcSpanInfo
type ImportDecl = A.ImportDecl SrcSpanInfo
type ImportSpecList = A.ImportSpecList SrcSpanInfo
type ImportSpec = A.ImportSpec SrcSpanInfo
type Decl = A.Decl SrcSpanInfo
type QName = A.QName SrcSpanInfo
type Name = A.Name SrcSpanInfo
type Type = A.Type SrcSpanInfo

class Display a where
    display :: a -> String

tildeBackup :: FilePath -> Maybe FilePath
tildeBackup = Just . (++ "~")

noBackup :: FilePath -> Maybe FilePath
noBackup = const Nothing

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path = (Just <$> readFile path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return Nothing else throw e)

removeFileIfPresent :: FilePath -> IO ()
removeFileIfPresent path = removeFile path `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)

replaceFileIfDifferent :: FilePath -> String -> IO Bool
replaceFileIfDifferent path newText =
    do oldText <- readFileMaybe path
       if oldText == Just newText then return False else replaceFile tildeBackup path newText >> return True

-- | Replace the file at path with the given text, moving the original
-- to the location returned by passing path to backup.  If backup is
-- the identity function you're going to have a bad time.
replaceFile :: (FilePath -> Maybe FilePath) -> FilePath -> String -> IO ()
replaceFile backup path text =
    remove >> rename >> write
    where
      remove = maybe (return ()) removeFile (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      rename = maybe (return ()) (renameFile path) (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      write = writeFile path text

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path action =
    bracket (getCurrentDirectory >>= \ save -> setCurrentDirectory path >> return save)
            setCurrentDirectory
            (const action)
