{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Modules.Params
    ( MonadClean
    , runMonadClean
    , modifyDryRun
    , modifyExtensions
    , modifyModuVerse
    , modifyHsFlags
    , modifySourceDirs
    , modifyRemoveEmptyImports
    , modifyTestMode
    ) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, MonadCatchIO, throw)
import Control.Monad.State (MonadState(get, put), StateT(runStateT))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Maybe (fromMaybe)
import Data.Set (empty, insert, Set, toList)
import qualified Language.Haskell.Exts.Annotated as A (Module, parseFileWithComments, parseFileWithMode)
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Extension (Extension)
import qualified Language.Haskell.Exts.Parser as Exts (defaultParseMode, ParseMode(extensions), ParseResult)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName)
import Language.Haskell.Modules.Common (modulePathBase)
import Language.Haskell.Modules.Internal
import Language.Haskell.Modules.Util.DryIO (MonadDryRun(..), createDirectoryIfMissing, replaceFile, tildeBackup, removeFileIfPresent)
import Language.Haskell.Modules.Util.QIO (MonadVerbosity(..), quietly, qPutStrLn, qPutStr)
import Language.Haskell.Modules.Util.Temp (withTempDirectory)
import Prelude hiding (writeFile)
import System.Directory (doesFileExist, getCurrentDirectory, removeFile)
import System.FilePath ((</>), dropExtension, takeDirectory)
import System.IO.Error (isDoesNotExistError)

-- | Controls whether file updates will actually be performed.  Default
-- is False.
modifyDryRun :: MonadClean m => (Bool -> Bool) -> m ()
modifyDryRun f = modifyParams (\ p -> p {dryRun = f (dryRun p)})

-- | Modify the extra extensions passed to the compiler and the
-- parser.  Default value is the list in
-- 'Language.Haskell.Exts.Parser.defaultParseMode'.
modifyExtensions :: MonadClean m => ([Extension] -> [Extension]) -> m ()
modifyExtensions f = modifyParams (\ p -> p {extensions = f (extensions p)})

-- | Modify the set of modules whose imports will be updated when
-- modules are split or merged.  No default, it is an error to run
-- splitModules or catModules without first setting this.
modifyModuVerse :: MonadClean m => (Set S.ModuleName -> Set S.ModuleName) -> m ()
modifyModuVerse f = modifyParams (\ p -> p {moduVerse = Just (f (fromMaybe empty (moduVerse p)))})

-- | Modify the list of extra flags passed to GHC.
modifyHsFlags :: MonadClean m => ([String] -> [String]) -> m ()
modifyHsFlags f = modifyParams (\ p -> p {hsFlags = f (hsFlags p)})

-- | Modify the list of directories that will be searched for source files.  Default is [\".\"].
modifySourceDirs :: MonadClean m => ([FilePath] -> [FilePath]) -> m ()
modifySourceDirs f = modifyParams (\ p -> p {sourceDirs = f (sourceDirs p)})

-- | Change the flag controlling whether imports that become empty will be
-- removed.  Default is True.
modifyRemoveEmptyImports :: MonadClean m => (Bool -> Bool) -> m ()
modifyRemoveEmptyImports f = modifyParams (\ p -> p {removeEmptyImports = f (removeEmptyImports p)})

-- | If TestMode is turned on no import cleaning will occur after a
-- split or cat.  Default is False.
modifyTestMode :: MonadClean m => (Bool -> Bool) -> m ()
modifyTestMode f = modifyParams (\ p -> p {testMode = f (testMode p)})
