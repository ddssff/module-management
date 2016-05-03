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
    ( moduleName
    , ModuVerseState, extensions, moduleInfo, moduleNames, sourceDirs
    , moduVerseInit
    , ModuVerse(..)
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
    ) where

import Control.Exception.Lifted as IO (catch, throw)
import Control.Lens (makeLenses)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Map as Map (delete, empty, insert, keys, lookup, Map)
import Data.Maybe (fromMaybe)
import Data.Set as Set (fromList, insert, minView, Set)
import qualified Language.Haskell.Exts.Annotated as A (Decl, Module(..), ModuleHead(..), ModuleName(..), parseFileWithComments)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import qualified Language.Haskell.Exts.Parser as Exts (defaultParseMode, fromParseResult, ParseMode(extensions, parseFilename, fixities), ParseResult)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax as S (ModuleName(..), Name)
import Language.Haskell.Modules.Fold (foldDecls, ModuleInfo(..))
import Language.Haskell.Modules.SourceDirs (modulePathBase, PathKey(..), Path(..), pathKey, SourceDirs(..))
import Language.Haskell.Modules.Util.QIO (MonadVerbosity, qLnPutStr, quietly)
import System.IO.Error (isDoesNotExistError, isUserError)

nameToExtension :: KnownExtension -> Extension
nameToExtension x = EnableExtension x

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
    ModuVerseState { _moduleNames :: Maybe (Map S.ModuleName ModuleInfo)
                   , _moduleInfo :: Map PathKey ModuleInfo
                   , _extensions :: [Extension]
                   , _sourceDirs :: [FilePath]
                   -- ^ Top level directories to search for source files and
                   -- imports.  These directories would be the value used in the
                   -- hs-source-dirs parameter of a cabal file, and passed to ghc
                   -- via the -i option.
                   } deriving (Eq, Ord, Show)

$(makeLenses ''ModuVerseState)

moduVerseInit :: ModuVerseState
moduVerseInit =
    ModuVerseState { _moduleNames = Nothing
                   , _moduleInfo = Map.empty
                   , _extensions = Exts.extensions Exts.defaultParseMode ++ [nameToExtension StandaloneDeriving] -- allExtensions
                   , _sourceDirs = ["."] }

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
getNames = getModuVerse >>= return . Set.fromList . keys . fromMaybe (error "No modules in ModuVerse, use putModule") . _moduleNames

getInfo :: ModuVerse m => S.ModuleName -> m (Maybe ModuleInfo)
getInfo name = getModuVerse >>= return . Map.lookup name . fromMaybe (error "No modules in ModuVerse, use putModule") . _moduleNames

putName :: ModuVerse m => S.ModuleName -> ModuleInfo -> m ()
putName name info = modifyModuVerse (\ s -> s {_moduleNames = Just (Map.insert name info (fromMaybe Map.empty (_moduleNames s)))})

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
delName name = modifyModuVerse (\ s -> s { _moduleNames = Just (Map.delete name (fromMaybe Map.empty (_moduleNames s)))
                                         , _moduleInfo = Map.empty })

class (MonadIO m, MonadBaseControl IO m, Functor m) => ModuVerse m where
    getModuVerse :: m ModuVerseState
    modifyModuVerse :: (ModuVerseState -> ModuVerseState) -> m ()

getExtensions :: ModuVerse m => m [Extension]
getExtensions = getModuVerse >>= return . _extensions

-- | Modify the list of extensions passed to GHC when dumping the
-- minimal imports.  Note that GHC will also use the extensions in the
-- module's LANGUAGE pragma, so this can usually be left alone.
modifyExtensions :: ModuVerse m => ([Extension] -> [Extension]) -> m ()
modifyExtensions f = modifyModuVerse (\ s -> s {_extensions = f (_extensions s)})

instance ModuVerse m => SourceDirs m where
    putHsSourceDirs xs = modifyModuVerse (\ s -> s {_sourceDirs = xs})
    getHsSourceDirs = getModuVerse >>= return . _sourceDirs

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
             return $ Map.lookup key (_moduleInfo verse)
      load (Just x) = return (Just x)
      load Nothing = Just <$> loadModule key

-- | Force a possibly cached module to be reloaded.
loadModule :: (ModuVerse m, MonadVerbosity m) => PathKey -> m ModuleInfo
loadModule key =
    do text <- liftIO $ readFile (unPathKey key)
       quietly $ qLnPutStr ("parsing " ++ unPathKey key)
       (parsed, comments) <- parseFileWithComments (unPathKey key) >>= return . Exts.fromParseResult
       modifyModuVerse (\ x -> x {_moduleInfo = Map.insert key (ModuleInfo parsed text comments key) (_moduleInfo x)})
       return (ModuleInfo parsed text comments key)

unloadModule :: (ModuVerse m, MonadVerbosity m) => PathKey -> m ()
unloadModule key =
    modifyModuVerse (\ x -> x {_moduleInfo = Map.delete key (_moduleInfo x)})

-- | Run 'A.parseFileWithComments' with the extensions stored in the state.
parseFileWithComments :: ModuVerse m => FilePath -> m (Exts.ParseResult (A.Module SrcSpanInfo, [Comment]))
parseFileWithComments path =
    liftIO (A.parseFileWithComments mode path)
    where
      mode = Exts.defaultParseMode {Exts.extensions = hseExtensions, Exts.parseFilename = path, Exts.fixities = Nothing }
