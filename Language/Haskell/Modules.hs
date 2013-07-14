-- | This package provides three functions.  The 'cleanImports'
-- function uses ghc's -ddump-minimal-imports flag to generate
-- minimized and explicit imports and re-insert them into the module.
--
-- The 'splitModuleDecls' function moves each declaration of a module
-- into a separate new module, and may also create three additional
-- modules: ReExported (for identifiers that were re-exported from
-- other imports), Instances (for declarations that don't result in an
-- identifier to export), and OtherSymbols (for declarations that
-- can't be turned into a module name.)
--
-- In addition to creating new modules, 'splitModuleDecls' also scans the
-- a set of modules (known as the moduVerse) and updates their imports
-- to account for the new locations of the symbols.  The moduVerse is
-- stored in MonadClean's state, and is updated as modules are created
-- and destroyed by 'splitModule' and 'catModules'.
--
-- The 'splitModule' function is a version of 'splitModuleDecls' that
-- allows the caller to customize the mapping from symbols to new
-- modules.
--
-- The 'mergeModules' function is the inverse operation of
-- 'splitModule', it merges two or more modules into a new or existing
-- module, updating imports of the moduVerse elements as necessary.
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
-- * 'mergeModules'
--
-- * 'runMonadClean' - Sets up the environment for splitting and merging
--
-- * 'Language.Haskell.Modules.Params' - Functions to control modes of operation
--
-- Examples:
--
-- * Use @cleanImports@ to clean up the import lists of all the modules under @./Language@:
--
--    @findPaths \"Language\" >>= runMonadClean . mapM cleanImports . toList@
--
-- * Use @splitModule@ to split up module
--   @Language.Haskell.Modules.Common@, and then merge two of the pieces
--   back in.
--
--   @findModules \"Language\" >>= \\ modules -> runMonadClean $
--      let mn = Language.Haskell.Exts.Syntax.ModuleName in
--      modifyModuVerse (const modules) >>
--      splitModule (mn \"Language.Haskell.Modules.Common\") >>
--      mergeModules (map mn [\"Language.Haskell.Modules.Common.WithCurrentDirectory\",
--                            \"Language.Haskell.Modules.Common.ModulePathBase\"])
--                   (mn \"Language.Haskell.Modules.Common\"))@
module Language.Haskell.Modules
    ( cleanImports
    , splitModule
    , splitModuleDecls
    , mergeModules
    , module Language.Haskell.Modules.Fold
    , module Language.Haskell.Modules.ModuVerse
    , module Language.Haskell.Modules.Params
    , module Language.Haskell.Modules.Util.QIO
    , findModules
    , findPaths
    ) where

import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldExports, foldHeader, foldImports, foldModule, ignore, ignore2)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Merge (mergeModules)
import Language.Haskell.Modules.ModuVerse (ModuVerse(..))
import Language.Haskell.Modules.Params (modifyDryRun, modifyHsFlags, modifyRemoveEmptyImports, modifyTestMode, runMonadClean)
import Language.Haskell.Modules.Split (splitModule, splitModuleDecls)
import Language.Haskell.Modules.Util.QIO (noisily, quietly)
import Language.Haskell.Modules.Util.Test (findModules, findPaths)
