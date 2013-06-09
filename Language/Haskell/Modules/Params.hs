{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Modules.Params
    ( Params(Params, dryRun, hsFlags, extensions, sourceDirs, junk, removeEmptyImports, scratchDir)
    , MonadClean
    , runCleanT
    , getParams
    , modifyParams
    , parseFileWithComments
    , parseFile
    , modulePath
    , markForDelete
    ) where

import Control.Applicative ((<$>))
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch, MonadCatchIO, throw)
import Control.Monad.State (MonadState(get, put), StateT(runStateT))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Set (empty, insert, Set, toList)
import qualified Language.Haskell.Exts.Annotated as A (Module, parseFileWithComments, parseFileWithMode)
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Extension (Extension)
import Language.Haskell.Exts.Parser as Exts (defaultParseMode, ParseMode(extensions), ParseResult)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName)
import Language.Haskell.Modules.Common (modulePathBase)
import Language.Haskell.Modules.Util.IO (removeFileIfPresent)
import Language.Haskell.Modules.Util.QIO (MonadVerbosity(..))
import Language.Haskell.Modules.Util.Temp (withTempDirectory)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))

data Params
    = Params
      { scratchDir :: FilePath
      -- ^ Location of the temporary directory for ghc output.
      , dryRun :: Bool -- unimplemented
      , verbosity :: Int
      , extensions :: [Extension]
      , hsFlags :: [String]
      -- ^ Extra flags to pass to GHC.
      , sourceDirs :: [FilePath]
      -- ^ Top level directories to search for source files and
      -- imports.  These directories would be the value used in the
      -- hs-source-dirs parameter of a cabal file, and passed to ghc
      -- via the -i option.
      , junk :: Set FilePath
      -- ^ Paths added to this list are removed as the monad finishes.
      , removeEmptyImports :: Bool
      -- ^ If true, remove any import that became empty due to the
      -- clean.  THe import might still be required because of the
      -- instances it contains, but usually it is not.  Note that this
      -- option does not affect imports that started empty and end
      -- empty.
      } deriving (Eq, Ord, Show)

class (MonadIO m, MonadCatchIO m, Functor m) => MonadClean m where
    getParams :: m Params
    putParams :: Params -> m ()

modifyParams :: MonadClean m => (Params -> Params) -> m ()
modifyParams f = getParams >>= putParams . f

instance (MonadCatchIO m, Functor m) => MonadClean (StateT Params m) where
    getParams = get
    putParams = put

instance MonadClean m => MonadVerbosity m where
    getVerbosity = getParams >>= return . verbosity
    putVerbosity v = getParams >>= \ p -> putParams (p {verbosity = v})

runCleanT :: MonadCatchIO m => StateT Params m a -> m a
runCleanT action =
    withTempDirectory "." "scratch" $ \ scratch ->
    do (result, params) <- runStateT action (Params {scratchDir = scratch,
                                                     dryRun = False,
                                                     verbosity = 0,
                                                     hsFlags = [],
                                                     Language.Haskell.Modules.Params.extensions = Exts.extensions defaultParseMode,
                                                     sourceDirs = ["."],
                                                     junk = empty,
                                                     removeEmptyImports = True})
       mapM_ (liftIO . removeFileIfPresent) (toList (junk params))
       return result

parseFileWithComments :: MonadClean m => FilePath -> m (ParseResult (A.Module SrcSpanInfo, [Comment]))
parseFileWithComments path =
    do exts <- getParams >>= return . Language.Haskell.Modules.Params.extensions
       liftIO (A.parseFileWithComments (defaultParseMode {Exts.extensions = exts}) path)

parseFile :: MonadClean m => FilePath -> m (ParseResult (A.Module SrcSpanInfo))
parseFile path =
    do exts <- getParams >>= return . Language.Haskell.Modules.Params.extensions
       liftIO (A.parseFileWithMode (defaultParseMode {Exts.extensions = exts}) path)

-- | Search the path directory list for a source file that already exists.
findSourcePath :: MonadClean m => FilePath -> m FilePath
findSourcePath path =
    findFile =<< (sourceDirs <$> getParams)
    where
      findFile (dir : dirs) =
          do let x = dir </> path
             exists <- liftIO $ doesFileExist x
             if exists then return x else findFile dirs
      findFile [] =
          do -- Just building an error message here
             here <- liftIO getCurrentDirectory
             dirs <- sourceDirs <$> getParams
             liftIO . throw . userError $ "findSourcePath failed, cwd=" ++ here ++ ", dirs=" ++ show dirs ++ ", path=" ++ path

-- | Search the path directory list, preferring an already existing file, but
-- if there is none construct one using the first element of the directory list.
modulePath :: MonadClean m => S.ModuleName -> m FilePath
modulePath name =
    findSourcePath (modulePathBase name) `catch` (\ (_ :: IOError) -> makePath)
    where
      makePath =
          do dirs <- sourceDirs <$> getParams
             case dirs of
               [] -> return (modulePathBase name) -- should this be an error?
               (d : _) -> return $ d </> modulePathBase name

markForDelete :: MonadClean m => FilePath -> m ()
markForDelete x = modifyParams (\ p -> p {junk = insert x (junk p)})
