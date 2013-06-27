-- | This package provides three functions.  The 'cleanImports'
-- function uses ghc's -ddump-minimal-imports flag to generate
-- minimized and explicit imports and re-insert them into the module.
--
-- The 'splitModule' moves each declaration of a module into a
-- separate new module, and may also create three additional modules:
-- ReExported (for identifiers that were re-exported from other
-- imports), Instances (for declarations that don't result in an
-- identifier to export), and OtherSymbols (for declarations that
-- can't be turned into a module name.)
--
-- In addition to creating new modules, 'splitModule' also scans the
-- a set of modules (known as the moduVerse) and updates their imports
-- to account for the new locations of the symbols.  The moduVerse is
-- stored in MonadClean's state, and is updated as modules are created
-- and destroyed by 'splitModule' and 'catModules'.
--
-- The 'catModules' function is the inverse operation of 'splitModule',
-- it merges two or more modules into a new or existing module, updating
-- imports of the moduVerse elements as necessary.
--
-- There are several features worth noting.  The 'Params' type in the
-- state of 'MonadClean' has a 'removeEmptyImports' field, which is
-- True by default.  This determines whether imports that turn into
-- empty lists are preserved or not - if your program needs instances
-- from a such an import, you will either want to set this flag to
-- False or (better) add an empty import list to the import.
--
-- These are the important entry points:
--
-- * 'cleanImports'
--
-- * 'splitModule'
--
-- * 'catModules'
--
-- * 'runCleanT', 'Params', 'modifyParams'
module Language.Haskell.Modules
    ( cleanImports
    , splitModule
    , catModules
    , runMonadClean
    , module Language.Haskell.Modules.Fold
    , Extension(..)
    , ModuleName(..)
{-
    -- * Clean, Split, and Cat
      cleanImports
    , splitModule
    , catModules
    -- * State and State Monad
    , Params(extensions, hsFlags, sourceDirs, moduVerse, removeEmptyImports, testMode, dryRun)
    , MonadClean
    , runCleanT
    , modifyParams
    -- Re-Exports from Language.Haskell.Exts.Syntax
    , Extension(..)
    , ModuleName(..)
    -- * Utility functions
    , foldModule
    , withCurrentDirectory
-}
    ) where

import Language.Haskell.Exts.Extension (Extension(..))
import Language.Haskell.Exts.Syntax (ModuleName(ModuleName))
import Language.Haskell.Modules.Cat (catModules)
import Language.Haskell.Modules.Fold (foldModule)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Split (splitModule)
import Language.Haskell.Modules.Params (Params(dryRun, hsFlags, extensions, sourceDirs, removeEmptyImports, testMode), MonadClean, runMonadClean, modifyParams)
