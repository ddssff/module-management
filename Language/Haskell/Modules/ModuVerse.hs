-- | The ModuVerse is the set of modules over which global operations
-- will be performed.  If a symbol moves from one module to another,
-- the imports of that symbol will also be updated across all the
-- modules tracked in the 'ModuVerseState'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.ModuVerse
    ( Params, dryRun, extraImports, hsFlags, junk, removeEmptyImports
    , scratchDir, verbosity{-, modulesOrig, moduleKey, modulesNew-}, moduVerse, moduleKeys
    , extensions, sourceDirs, destinationMap, symbolMap, cleanMode, moveFunction
    , CleanMode(DoClean, NoClean)
    , ModuVerse
    , runModuVerseT
    , markForDelete
    , extraImport
    , moduleName
    , putModule
    , putModuleAnew
    , findModule
    -- , findSymbolDecl
    -- , delName
    , getExtensions
    , modifyExtensions
    , parseModule
    , parseModuleMaybe
    , loadModule
    , unloadModule
    , buildSymbolMap
    , buildDestinationMap
    ) where

import Control.Exception (SomeException, try)
import Control.Lens (makeLenses)
import Control.Monad as List (foldM, when)
import Control.Monad.Trans.Control as IO (MonadBaseControl)
import Control.Monad.State (MonadState, StateT(runStateT))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.List as List (foldl')
import Data.Map as Map (adjust, empty, insertWith, Map, toList)
import Data.Monoid ((<>))
import Data.Set as Set (delete, empty, insert, Set, singleton, toList, union)
import Language.Haskell.Exts.SrcLoc (SrcLoc(SrcLoc))
import Language.Haskell.Exts.Syntax as S (ImportDecl(..), ModuleName)
import Language.Haskell.Modules.Util.DryIO (MonadDryRun(..))
import Language.Haskell.Modules.Util.QIO (MonadVerbosity(..))
import Language.Haskell.Modules.Util.Temp (withTempDirectory)
import Prelude hiding (writeFile, writeFile)
import System.Directory (removeFile)

import Control.Exception.Lifted as IO (catch, throw)
import Control.Lens (use, (%=), (.=))
import Data.Map as Map (delete, insert, keys, lookup)
import qualified Language.Haskell.Exts.Annotated as A (Decl, Module(..), ModuleHead(..), ModuleName(..), parseFileWithComments)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import qualified Language.Haskell.Exts.Parser as Exts (defaultParseMode, fromParseResult, ParseMode(extensions, parseFilename, fixities), ParseResult)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax as S (ModuleName(..), Name)
import Language.Haskell.Modules.FoldM (foldDeclsM, ModuleInfo(..))
import Language.Haskell.Modules.SourceDirs (modulePathBase, ModKey(..), Path(..), modKey, SourceDirs(..))
import Language.Haskell.Modules.Symbols (symbolsDeclaredBy)
--import Language.Haskell.Modules.Util.QIO (qLnPutStr, quietly)
import System.IO.Error (isDoesNotExistError, isUserError)

data CleanMode = DoClean | NoClean deriving (Eq, Ord, Show)

-- | This contains the information required to run the state monad for
-- import cleaning and module spliting/mergeing.
data Params
    = Params
      { _scratchDir :: FilePath
      -- ^ Location of the temporary directory for ghc output.
      , _dryRun :: Bool
      -- ^ None of the operations that modify the modules will actually
      -- be performed if this is ture.
      , _verbosity :: Int
      -- ^ Increase or decrease the amount of progress reporting.
      , _hsFlags :: [String]
      -- ^ Extra flags to pass to GHC.
      , _extensions :: [Extension]
      , _sourceDirs :: [FilePath]
      -- ^ Top level directories to search for source files and
      -- imports.  These directories would be the value used in the
      -- hs-source-dirs parameter of a cabal file, and passed to ghc
      -- via the -i option.
      , _junk :: Set FilePath
      -- ^ Paths added to this list are removed as the state monad
      -- finishes.
      , _removeEmptyImports :: Bool
      -- ^ If true, remove any import that became empty due to the
      -- clean.  THe import might still be required because of the
      -- instances it contains, but usually it is not.  Note that this
      -- option does not affect imports that started empty and end
      -- empty.
      , _extraImports :: Map S.ModuleName [S.ImportDecl]
      -- ^ Deciding whether a module needs to be imported can be
      -- difficult when instances are involved, this is a cheat to force
      -- keys of the map to import the corresponding elements.
      , _cleanMode :: CleanMode
      -- ^ For testing, do not run cleanImports on the results of the
      -- splitModule and catModules operations.

      , _moduVerse :: Map ModKey ModuleInfo
      -- ^ The info about the modules as they were loaded from disk,
      -- associated with the key used to find the module.
      , _moduleKeys :: Map S.ModuleName (Set ModKey)
      -- ^ Map from module names back to the path
{-
      , _modulesOrig :: Map ModKey ModuleInfo
      -- ^ The info about the modules as they were loaded from disk,
      -- associated with the key used to find the module.
-}
{-
      , _modulesNew :: Map S.ModuleName ModuleInfo
      -- ^ The modified modules.  When we are modifying modules we
      -- refer to them by module name, which for these computations
      -- must be unique.  the key that was originally used to obtain
      -- the module is stored in ModuleInfo so we can later go back
-}
      -- and see what changes occurred.
      , _symbolMap :: Map (ModKey, S.Name) (A.Decl SrcSpanInfo)
      -- ^ Look up a symbol's declaration in a particular module.
      , _destinationMap :: Map (ModKey, A.Decl SrcSpanInfo) ModKey
      -- ^ Look up where a declaration will be moved to
      , _moveFunction :: (ModKey -> A.Decl SrcSpanInfo -> ModKey)
      -- ^ Function @(\ m d -> m')@ describing for a declaration d
      -- originating in module m what module to move it into.
      }


data ModuleStatus = Unchanged | Modified | Created | Removed deriving (Eq, Ord, Show)

$(makeLenses ''Params)

class (MonadIO m, MonadBaseControl IO m, Functor m, MonadState Params m) => ModuVerse m

instance (MonadIO m, MonadBaseControl IO m, Functor m) => ModuVerse (StateT Params m)

instance ModuVerse m => MonadVerbosity m where
    getVerbosity = use verbosity
    putVerbosity v = verbosity .= v

instance ModuVerse m => MonadDryRun m where
    dry = use dryRun
    putDry x = dryRun .= x

instance ModuVerse m => SourceDirs m where
    putHsSourceDirs xs = sourceDirs .= xs
    getHsSourceDirs = use sourceDirs

-- | Create the environment required to do import cleaning and module
-- splitting/merging.  This environment, @StateT Params m a@, is an
-- instance of 'ModuVerse'.
runModuVerseT :: (MonadIO m, MonadBaseControl IO m) => StateT Params m a -> m a
runModuVerseT action =
    withTempDirectory "." "hmm" $ \ scratch ->
    do (result, params) <- runStateT action (Params {_scratchDir = scratch,
                                                     _dryRun = False,
                                                     _verbosity = 1,
                                                     _hsFlags = [],
                                                     _moduVerse = Map.empty,
                                                     _moduleKeys = Map.empty,
{-
                                                     _modulesOrig = Map.empty,
                                                     _modulesNew = Map.empty,
-}
                                                     _extensions = Exts.extensions Exts.defaultParseMode ++
                                                                   [nameToExtension StandaloneDeriving], -- allExtensions
                                                     _sourceDirs = ["."],
                                                     _symbolMap = Map.empty,
                                                     _destinationMap = Map.empty,
                                                     _cleanMode = DoClean,
                                                     _junk = Set.empty,
                                                     _removeEmptyImports = True,
                                                     _extraImports = Map.empty,
                                                     _moveFunction = \m _ -> m
                                                    })
       mapM_ (\ x -> liftIO (try (removeFile x)) >>= \ (_ :: Either SomeException ()) -> return ()) (Set.toList (_junk params))
       return result

markForDelete :: ModuVerse m => FilePath -> m ()
markForDelete x = junk %= Set.insert x

-- | When we write module @m@, insert an extra line that imports the
-- instances (only) from module @i@.
extraImport :: ModuVerse m => S.ModuleName -> S.ModuleName -> m ()
extraImport m i =
    extraImports %= Map.insertWith (<>) m [im]
    where im = ImportDecl { importLoc = SrcLoc "<unknown>.hs" 1 1
                          , importModule = i
                          , importQualified = False
                          , importSrc = False
                          , importSafe = False
                          , importPkg = Nothing
                          , importAs = Nothing
                          , importSpecs = Just (False, []) }

nameToExtension :: KnownExtension -> Extension
nameToExtension x = EnableExtension x

moduleName :: ModuleInfo -> S.ModuleName
moduleName (ModuleInfo (A.Module _ mh _ _ _) _ _ _ _) =
    S.ModuleName $ maybe "Main" (\ (A.ModuleHead _ (A.ModuleName _ s) _ _) -> s) mh
moduleName (ModuleInfo m _ _ _ _) = error $ "Unsupported Module: " ++ show m

-- | From hsx2hs, but removing Arrows because it makes test case fold3c and others fail.
hseExtensions :: [Extension]
hseExtensions = map nameToExtension
    [ RecursiveDo, ParallelListComp, MultiParamTypeClasses, FunctionalDependencies, RankNTypes, ExistentialQuantification
    , ScopedTypeVariables, ImplicitParams, FlexibleContexts, FlexibleInstances, EmptyDataDecls, KindSignatures
    , BangPatterns, TemplateHaskell, ForeignFunctionInterface, {- Arrows, -} Generics, NamedFieldPuns, PatternGuards
    , MagicHash, TypeFamilies, StandaloneDeriving, TypeOperators, RecordWildCards, GADTs, UnboxedTuples
    , PackageImports, QuasiQuotes, {-TransformListComp,-} ViewPatterns, XmlSyntax, RegularPatterns, TupleSections
    , ExplicitNamespaces
    ]

putModule :: ModuVerse m => S.ModuleName -> m ()
putModule name = do
  key <- modKey (modulePathBase "hs" name)
  info <- parseModule key
  moduVerse %= Map.insert key info
  moduleKeys %= Map.insertWith union name (singleton key)
{-
  modulesOrig %= Map.insert key info
-}

-- | Update the ModuVerse info for a module by re-reading its (perhaps
-- altered) source text.
putModuleAnew :: ModuVerse m => ModKey -> m ()
putModuleAnew key = do
  -- key <- modKey (modulePathBase "hs" name)
  info <- loadModule key
  moduVerse %= Map.insert key info
  moduleKeys %= Map.insertWith union (unModName key) (singleton key)
{-
  modulesOrig %= Map.insert key info
  modulesNew %= Map.delete name
-}

findModule :: ModuVerse m => S.ModuleName -> m (Maybe ModuleInfo)
findModule name = modKeyMaybe (modulePathBase "hs" name) >>= parseModuleMaybe

{-
-- | Given a symbol name and the module from which it can be imported,
-- return the Decl that creates it.
findSymbolDecl :: ModuVerse m => S.ModuleName -> S.Name -> m (Maybe (A.Decl SrcSpanInfo))
findSymbolDecl modu name =
    do Just info <- Map.lookup modu <$> use modulesNew
       let s = foldDecls (\d _ _ _ r -> Set.insert d r) (\_ r -> r) info mempty
       case Set.minView s of
         Nothing -> return Nothing
         Just (d, s') | null s' -> return (Just d)
         Just _ -> error ("Multiple declarations of " ++ show name ++ " in " ++ show modu)

delName :: ModuVerse m => S.ModuleName -> m ()
delName name = do
  moduleInfo %= Map.delete name
  modulesOrig .= Map.empty
-}

getExtensions :: ModuVerse m => m [Extension]
getExtensions = use extensions

-- | Modify the list of extensions passed to GHC when dumping the
-- minimal imports.  Note that GHC will also use the extensions in the
-- module's LANGUAGE pragma, so this can usually be left alone.
modifyExtensions :: ModuVerse m => ([Extension] -> [Extension]) -> m ()
modifyExtensions f = extensions %= f

parseModule :: (ModuVerse m, MonadVerbosity m) => ModKey -> m ModuleInfo
parseModule key = parseModuleMaybe (Just key) >>= maybe (error $ "parseModule - not found: " ++ show key) return

parseModuleMaybe :: (ModuVerse m, MonadVerbosity m) => Maybe ModKey -> m (Maybe ModuleInfo)
parseModuleMaybe Nothing = return Nothing
parseModuleMaybe (Just key) =
    (look >>= load) `IO.catch` (\ (e :: IOError) -> if isDoesNotExistError e || isUserError e then return Nothing else throw e)
    where
      look = Map.lookup key <$> use moduVerse
      load (Just x) = return (Just x)
      load Nothing = Just <$> loadModule key

-- | Force a possibly cached module to be reloaded.
loadModule :: (ModuVerse m, MonadVerbosity m) => ModKey -> m ModuleInfo
loadModule key@(ModKey _ name) =
    do text <- liftIO $ readFile (unModKey key)
       -- quietly $ qLnPutStr ("parsing " ++ unModKey key)
       (parsed, comments) <- parseFileWithComments (unModKey key) >>= return . Exts.fromParseResult
       moduVerse %= Map.insert key (ModuleInfo parsed text comments key name)
       -- modulesOrig %= Map.insert key (ModuleInfo parsed text comments key name)
       moduleKeys %= Map.insertWith union name (singleton key)
       return (ModuleInfo parsed text comments key name)

unloadModule :: (ModuVerse m, MonadVerbosity m) => ModKey -> m ()
unloadModule key@(ModKey _ name) = do
  moduVerse %= Map.delete key
  moduleKeys %= Map.adjust (Set.delete key) name

-- | Run 'A.parseFileWithComments' with the extensions stored in the state.
parseFileWithComments :: ModuVerse m => FilePath -> m (Exts.ParseResult (A.Module SrcSpanInfo, [Comment]))
parseFileWithComments path =
    liftIO (A.parseFileWithComments mode path)
    where
      mode = Exts.defaultParseMode {Exts.extensions = hseExtensions, Exts.parseFilename = path, Exts.fixities = Nothing }

buildDestinationMap :: forall m. ModuVerse m => m ()
buildDestinationMap = do
  destinationMap .= mempty
  (Map.toList <$> use moduVerse) >>= mapM_ doModule
    where
      doModule (key, info) = do
        -- keys <- Map.lookup oldModule <$> use moduleKeys
        -- Just oldInfo <- Map.lookup key <$> use moduVerse
        newModule <- use moveFunction
        foldDeclsM (\d _ _ _ _ -> do
                      let m = newModule key d
                      when (key /= m)
                           (do destinationMap %= Map.insert (key, d) m
                               -- originMap %= Map.insertWith Set.union destinationModuleName (singleton originInfo)
                               -- declMap %= Map.insertWith (flip (<>)) (newModule (name_ originInfo) d) [(d, pref <> s <> suff)]
                           ))
                   (\_ _ -> pure ())
                   info ()

buildSymbolMap :: forall m. ModuVerse m => m ()
buildSymbolMap = do
  symbolMap .= mempty
  -- ks <- keys <$> use moduVerse
  -- names <- (map name_ . elems) <$> use moduVerse
  mp <- (keys <$> use moduVerse) >>= foldM doModule mempty
  symbolMap .= mp
    where
      doModule mp oldModule = do
        -- keys <- Map.lookup oldModule <$> use moduVerse
        Just oldInfo <- Map.lookup oldModule <$> use moduVerse
        foldDeclsM (\d _ _ _ r -> pure $ foldl' (\mp' s -> Map.insert (oldModule, s) d mp') r (symbolsDeclaredBy d))
                   (\_ r -> pure r)
                   oldInfo mp
