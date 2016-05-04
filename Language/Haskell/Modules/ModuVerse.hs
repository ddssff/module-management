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
    , scratchDir, verbosity, moduleNames, moduleInfo, extensions, sourceDirs
    , declMap, symbolMap, testMode
    , CleanMode(DoClean, NoClean)
    , ModuVerse
    , runModuVerseT
    , markForDelete
    , extraImport
    , moduleName
    -- , ModuVerse(..)
    , getNames
    , getInfo
    -- , putName
    , putModule
    , putModuleAnew
    , findModule
    , findSymbolDecl
    , delName
    , getExtensions
    , modifyExtensions
    -- , getSourceDirs
    -- , modifySourceDirs
    , parseModule
    , parseModuleMaybe
    , loadModule
    , unloadModule
    , buildSymbolMap
    , buildDeclMap
    ) where

import Control.Exception (SomeException, try)
import Control.Lens (makeLenses)
import Control.Monad as List (foldM)
import Control.Monad.Trans.Control as IO (MonadBaseControl)
import Control.Monad.State (MonadState, StateT(runStateT))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.List as List (foldl')
import Data.Map as Map (empty, Map, insertWith)
import Data.Monoid ((<>))
import Data.Set as Set (empty, insert, Set, toList)
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
import Data.Maybe (fromMaybe)
import Data.Set as Set (fromList, minView)
import qualified Language.Haskell.Exts.Annotated as A (Decl, Module(..), ModuleHead(..), ModuleName(..), parseFileWithComments)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import qualified Language.Haskell.Exts.Parser as Exts (defaultParseMode, fromParseResult, ParseMode(extensions, parseFilename, fixities), ParseResult)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax as S (ModuleName(..), Name)
import Language.Haskell.Modules.Fold (foldDecls, ModuleInfo(..))
import Language.Haskell.Modules.SourceDirs (modulePathBase, PathKey(..), Path(..), pathKey, SourceDirs(..))
import Language.Haskell.Modules.Symbols (symbolsDeclaredBy)
import Language.Haskell.Modules.Util.QIO (qLnPutStr, quietly)
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
      , _moduleNames :: Maybe (Map S.ModuleName ModuleInfo)
      , _moduleInfo :: Map PathKey ModuleInfo
      -- ^ The set of modules that splitModules and catModules will
      -- check for imports of symbols that moved.
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
      , _testMode :: CleanMode
      -- ^ For testing, do not run cleanImports on the results of the
      -- splitModule and catModules operations.
      , _symbolMap :: Map (S.ModuleName, S.Name) (A.Decl SrcSpanInfo)
      -- ^ Look up a symbol's declaration
      , _declMap :: Map (S.ModuleName, A.Decl SrcSpanInfo) S.ModuleName
      -- ^ Look up where a declaration will be moved to
      } deriving (Eq, Ord, Show)

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
                                                     _moduleNames = Nothing,
                                                     _moduleInfo = Map.empty,
                                                     _extensions = Exts.extensions Exts.defaultParseMode ++
                                                                   [nameToExtension StandaloneDeriving], -- allExtensions
                                                     _sourceDirs = ["."],
                                                     _symbolMap = Map.empty,
                                                     _declMap = Map.empty,
                                                     _testMode = DoClean,
                                                     _junk = Set.empty,
                                                     _removeEmptyImports = True,
                                                     _extraImports = Map.empty})
       mapM_ (\ x -> liftIO (try (removeFile x)) >>= \ (_ :: Either SomeException ()) -> return ()) (toList (_junk params))
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
moduleName (ModuleInfo (A.Module _ mh _ _ _) _ _ _) =
    S.ModuleName $ maybe "Main" (\ (A.ModuleHead _ (A.ModuleName _ s) _ _) -> s) mh
moduleName (ModuleInfo m _ _ _) = error $ "Unsupported Module: " ++ show m

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

getNames :: ModuVerse m => m (Set S.ModuleName)
getNames = Set.fromList . keys . fromMaybe (error "No modules in ModuVerse, use putModule") <$> use moduleNames

getInfo :: ModuVerse m => S.ModuleName -> m (Maybe ModuleInfo)
getInfo name = Map.lookup name . fromMaybe (error "No modules in ModuVerse, use putModule") <$> use moduleNames

putName :: ModuVerse m => S.ModuleName -> ModuleInfo -> m ()
putName name info = moduleNames %= Just . Map.insert name info . fromMaybe Map.empty

putModule :: ModuVerse m => S.ModuleName -> m ()
putModule name = pathKey (modulePathBase "hs" name) >>= parseModule >>= putName name

-- | Update the ModuVerse info for a module by re-reading its (perhaps
-- altered) source text.
putModuleAnew :: ModuVerse m => S.ModuleName -> m PathKey
putModuleAnew name =
    do key <- pathKey (modulePathBase "hs" name)
       loadModule key >>= putName name
       return key

findModule :: ModuVerse m => S.ModuleName -> m (Maybe ModuleInfo)
findModule name = pathKeyMaybe (modulePathBase "hs" name) >>= parseModuleMaybe

-- | Given a symbol name and the module from which it can be imported,
-- return the Decl that creates it.
findSymbolDecl :: ModuVerse m => S.ModuleName -> S.Name -> m (Maybe (A.Decl SrcSpanInfo))
findSymbolDecl modu name =
    do Just info <- getInfo modu
       let s = foldDecls (\d _ _ _ r -> Set.insert d r) (\_ r -> r) info mempty
       case Set.minView s of
         Nothing -> return Nothing
         Just (d, s') | null s' -> return (Just d)
         Just _ -> error ("Multiple declarations of " ++ show name ++ " in " ++ show modu)

delName :: ModuVerse m => S.ModuleName -> m ()
delName name = do
  moduleNames %= Just . Map.delete name . fromMaybe Map.empty
  moduleInfo .= Map.empty

getExtensions :: ModuVerse m => m [Extension]
getExtensions = use extensions

-- | Modify the list of extensions passed to GHC when dumping the
-- minimal imports.  Note that GHC will also use the extensions in the
-- module's LANGUAGE pragma, so this can usually be left alone.
modifyExtensions :: ModuVerse m => ([Extension] -> [Extension]) -> m ()
modifyExtensions f = extensions %= f

parseModule :: (ModuVerse m, MonadVerbosity m) => PathKey -> m ModuleInfo
parseModule key = parseModuleMaybe (Just key) >>= maybe (error $ "parseModule - not found: " ++ show key) return

parseModuleMaybe :: (ModuVerse m, MonadVerbosity m) => Maybe PathKey -> m (Maybe ModuleInfo)
parseModuleMaybe Nothing = return Nothing
parseModuleMaybe (Just key) =
    (look >>= load) `IO.catch` (\ (e :: IOError) -> if isDoesNotExistError e || isUserError e then return Nothing else throw e)
    where
      look = Map.lookup key <$> use moduleInfo
      load (Just x) = return (Just x)
      load Nothing = Just <$> loadModule key

-- | Force a possibly cached module to be reloaded.
loadModule :: (ModuVerse m, MonadVerbosity m) => PathKey -> m ModuleInfo
loadModule key =
    do text <- liftIO $ readFile (unPathKey key)
       quietly $ qLnPutStr ("parsing " ++ unPathKey key)
       (parsed, comments) <- parseFileWithComments (unPathKey key) >>= return . Exts.fromParseResult
       moduleInfo %= Map.insert key (ModuleInfo parsed text comments key)
       return (ModuleInfo parsed text comments key)

unloadModule :: (ModuVerse m, MonadVerbosity m) => PathKey -> m ()
unloadModule key = moduleInfo %= Map.delete key

-- | Run 'A.parseFileWithComments' with the extensions stored in the state.
parseFileWithComments :: ModuVerse m => FilePath -> m (Exts.ParseResult (A.Module SrcSpanInfo, [Comment]))
parseFileWithComments path =
    liftIO (A.parseFileWithComments mode path)
    where
      mode = Exts.defaultParseMode {Exts.extensions = hseExtensions, Exts.parseFilename = path, Exts.fixities = Nothing }

buildDeclMap :: forall m. ModuVerse m =>
                (S.ModuleName -> A.Decl SrcSpanInfo -> S.ModuleName)
             -> m ()
buildDeclMap newModule = do
  names <- Set.toList <$> getNames :: m [S.ModuleName]
  mp <- foldM doModule mempty names
  declMap .= mp
    where
      doModule mp oldModule = do
        Just oldInfo <- getInfo oldModule
        let mp' = foldDecls (\d _ _ _ r ->
                               let m :: S.ModuleName
                                   m = newModule oldModule d in
                               if oldModule /= m
                               then (Map.insert (oldModule, d) m r :: Map (S.ModuleName, A.Decl SrcSpanInfo) S.ModuleName)
                               else r)
                            (\_ r -> r)
                            oldInfo mp
        return mp'

buildSymbolMap :: forall m. ModuVerse m => m ()
buildSymbolMap = do
  symbolMap .= mempty
  names <- Set.toList <$> getNames :: m [S.ModuleName]
  mp <- foldM doModule mempty names
  symbolMap .= mp
    where
      doModule mp oldModule = do
        Just oldInfo <- getInfo oldModule
        let mp'' = foldDecls (\d _ _ _ r -> foldl' (\mp' s -> Map.insert (oldModule, s) d mp') r (symbolsDeclaredBy d))
                             (\_ r -> r)
                             oldInfo mp
        return mp''
