-- | The ModuVerse is the set of modules over which global operations
-- will be performed.  If a symbol moves from one module to another,
-- the imports of that symbol will also be updated across all the
-- modules tracked in the 'ModuVerseState'.
{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, PackageImports, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.ModuVerse
    ( ModuleInfo(..)
    , moduleName
    , ModuVerseState
    , moduVerseInit
    , ModuVerse(..)
    , getNames
    , getInfo
    -- , putName
    , putModule
    , putModuleAnew
    , findModule
    , delName
    , getExtensions
    , modifyExtensions
    -- , getSourceDirs
    -- , modifySourceDirs
    , parseModule
    , parseModuleMaybe
    , loadModule
    , unloadModule
    ) where

import Control.Exception.Lifted as IO (catch, throw)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Map as Map (delete, empty, insert, keys, lookup, Map)
import Data.Maybe (fromMaybe)
import Data.Set as Set (fromList, Set)
import qualified Language.Haskell.Exts.Annotated as A (Module(..), ModuleHead(..), ModuleName(..), parseFileWithComments)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import qualified Language.Haskell.Exts.Parser as Exts (defaultParseMode, fromParseResult, ParseMode(extensions, parseFilename, fixities), ParseResult)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax as S (ModuleName(..))
import Language.Haskell.Modules.SourceDirs (modulePathBase, PathKey(..), Path(..), pathKey, SourceDirs(..))
import Language.Haskell.Modules.Util.QIO (MonadVerbosity, qLnPutStr, quietly)
import System.IO.Error (isDoesNotExistError, isUserError)

nameToExtension :: KnownExtension -> Extension
nameToExtension x = EnableExtension x

deriving instance Ord Comment

data ModuleInfo
    = ModuleInfo
      { module_ :: A.Module SrcSpanInfo
      , text_ :: String
      , comments_ :: [Comment]
      , key_ :: PathKey }
    deriving (Eq, Ord, Show)

{-
moduleName :: A.Module a -> S.ModuleName
moduleName (A.Module _ (Just (A.ModuleHead _ x _ _)) _ _ _) = sModuleName x
moduleName _ = S.ModuleName "Main"
-}

moduleName :: ModuleInfo -> S.ModuleName
moduleName (ModuleInfo (A.Module _ mh _ _ _) _ _ _) =
    S.ModuleName $ maybe "Main" (\ (A.ModuleHead _ (A.ModuleName _ s) _ _) -> s) mh
moduleName (ModuleInfo m _ _ _) = error $ "Unsupported Module: " ++ show m

data ModuVerseState =
    ModuVerseState { moduleNames_ :: Maybe (Map S.ModuleName ModuleInfo)
                   , moduleInfo_ :: Map PathKey ModuleInfo
                   , extensions_ :: [Extension]
                   , sourceDirs_ :: [FilePath]
                   -- ^ Top level directories to search for source files and
                   -- imports.  These directories would be the value used in the
                   -- hs-source-dirs parameter of a cabal file, and passed to ghc
                   -- via the -i option.
                   } deriving (Eq, Ord, Show)

moduVerseInit :: ModuVerseState
moduVerseInit =
    ModuVerseState { moduleNames_ = Nothing
                   , moduleInfo_ = Map.empty
                   , extensions_ = Exts.extensions Exts.defaultParseMode ++ [nameToExtension StandaloneDeriving] -- allExtensions
                   , sourceDirs_ = ["."] }

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
getNames = getModuVerse >>= return . Set.fromList . keys . fromMaybe (error "No modules in ModuVerse, use putModule") . moduleNames_

getInfo :: ModuVerse m => S.ModuleName -> m (Maybe ModuleInfo)
getInfo name = getModuVerse >>= return . Map.lookup name . fromMaybe (error "No modules in ModuVerse, use putModule") . moduleNames_

putName :: ModuVerse m => S.ModuleName -> ModuleInfo -> m ()
putName name info = modifyModuVerse (\ s -> s {moduleNames_ = Just (Map.insert name info (fromMaybe Map.empty (moduleNames_ s)))})

putModule :: (ModuVerse m, MonadVerbosity m) => S.ModuleName -> m ()
putModule name = pathKey (modulePathBase "hs" name) >>= parseModule >>= putName name

-- | Update the ModuVerse info for a module by re-reading its (perhaps
-- altered) source text.
putModuleAnew :: (ModuVerse m, MonadVerbosity m) => S.ModuleName -> m PathKey
putModuleAnew name =
    do key <- pathKey (modulePathBase "hs" name)
       loadModule key >>= putName name
       return key

findModule :: (ModuVerse m, MonadVerbosity m) => S.ModuleName -> m (Maybe ModuleInfo)
findModule name = pathKeyMaybe (modulePathBase "hs" name) >>= parseModuleMaybe

delName :: ModuVerse m => S.ModuleName -> m ()
delName name = modifyModuVerse (\ s -> s { moduleNames_ = Just (Map.delete name (fromMaybe Map.empty (moduleNames_ s)))
                                         , moduleInfo_ = Map.empty })

class (MonadIO m, MonadBaseControl IO m, Functor m) => ModuVerse m where
    getModuVerse :: m ModuVerseState
    modifyModuVerse :: (ModuVerseState -> ModuVerseState) -> m ()

getExtensions :: ModuVerse m => m [Extension]
getExtensions = getModuVerse >>= return . extensions_

-- | Modify the list of extensions passed to GHC when dumping the
-- minimal imports.  Note that GHC will also use the extensions in the
-- module's LANGUAGE pragma, so this can usually be left alone.
modifyExtensions :: ModuVerse m => ([Extension] -> [Extension]) -> m ()
modifyExtensions f = modifyModuVerse (\ s -> s {extensions_ = f (extensions_ s)})

instance ModuVerse m => SourceDirs m where
    putHsSourceDirs xs = modifyModuVerse (\ s -> s {sourceDirs_ = xs})
    getHsSourceDirs = getModuVerse >>= return . sourceDirs_

{-
getSourceDirs :: ModuVerse m => m [FilePath]
getSourceDirs = getModuVerse >>= return . sourceDirs_

-- | Modify the list of directories that will be searched for source
-- files, in a similar way to the Hs-Source-Dirs field in a cabal
-- file.  Default is @[\".\"]@.
modifySourceDirs :: ModuVerse m => ([FilePath] -> [FilePath]) -> m ()
modifySourceDirs f = modifyModuVerse (\ p -> p {sourceDirs_ = f (sourceDirs_ p)})
-}

parseModule :: (ModuVerse m, MonadVerbosity m) => PathKey -> m ModuleInfo
parseModule key = parseModuleMaybe (Just key) >>= maybe (error $ "parseModule - not found: " ++ show key) return

parseModuleMaybe :: (ModuVerse m, MonadVerbosity m) => Maybe PathKey -> m (Maybe ModuleInfo)
parseModuleMaybe Nothing = return Nothing
parseModuleMaybe (Just key) =
    (look >>= load) `IO.catch` (\ (e :: IOError) -> if isDoesNotExistError e || isUserError e then return Nothing else throw e)
    where
      look =
          do verse <- getModuVerse
             return $ Map.lookup key (moduleInfo_ verse)
      load (Just x) = return (Just x)
      load Nothing = Just <$> loadModule key

-- | Force a possibly cached module to be reloaded.
loadModule :: (ModuVerse m, MonadVerbosity m) => PathKey -> m ModuleInfo
loadModule key =
    do text <- liftIO $ readFile (unPathKey key)
       quietly $ qLnPutStr ("parsing " ++ unPathKey key)
       (parsed, comments) <- parseFileWithComments (unPathKey key) >>= return . Exts.fromParseResult
       modifyModuVerse (\ x -> x {moduleInfo_ = Map.insert key (ModuleInfo parsed text comments key) (moduleInfo_ x)})
       return (ModuleInfo parsed text comments key)

unloadModule :: (ModuVerse m, MonadVerbosity m) => PathKey -> m ()
unloadModule key =
    modifyModuVerse (\ x -> x {moduleInfo_ = Map.delete key (moduleInfo_ x)})

-- | Run 'A.parseFileWithComments' with the extensions stored in the state.
parseFileWithComments :: ModuVerse m => FilePath -> m (Exts.ParseResult (A.Module SrcSpanInfo, [Comment]))
parseFileWithComments path =
    liftIO (A.parseFileWithComments mode path)
    where
      mode = Exts.defaultParseMode {Exts.extensions = hseExtensions, Exts.parseFilename = path, Exts.fixities = Nothing }
