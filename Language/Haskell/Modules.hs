-- | This package provides functions to clean import lists, to split
-- up modules, and to merge modules.    The important entry
-- points are:
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
-- * 'runCleanT' - Sets up the environment for splitting and merging.
--   These operations require updates to be made to all the modules
--   that import the modules being split or merged, so this
--   environment tracks the creation and removal of modules.  This
--   allows a sequence of splits and merges to be performed without
--   forgetting to update newly created modules.
--
-- * 'cleanImports' - uses ghc's -ddump-minimal-imports flag to
--   generate minimized and explicit imports and re-insert them into
--   the module.
--
-- * 'splitModule' - Splits a module into two or more parts according to
--   the argument function.
--
-- * 'splitModuleDecls' - Calls 'splitModule' with a default first
--   argument.  Each declaration goes into a different module, and
--   separate modules are created for instances and re-exports.  Decls
--   that were local to the original module go into a subdirectory named
--   @Internal@.  Symbols which can't be turned into valid module names
--   go into @OtherSymbols@.
--
-- * 'mergeModules' - the inverse operation of 'splitModule', it
--   merges two or more modules into a new or existing module, updating
--   imports of the moduVerse elements as necessary.
--
-- * 'Language.Haskell.Modules.Params' - Functions to control modes of operation
--
-- Examples:
--
-- * Use 'findHsFiles' and 'cleanImports' to clean up the import lists
-- of all the modules under @./Language@:
--
--    @findHsFiles [\"Language\", \"Tests.hs\", \"Tests\"] >>= runCleanT . cleanImports . toList@
--
-- * Use 'findHsModules' and 'splitModule' to split up module
--   @Language.Haskell.Modules.Common@, and then merge two of the pieces
--   back in.
--
--   @findHsModules [\"Language\", \"Tests.hs\", \"Tests\"] >>= \\ modules -> runCleanT $
--      let mn = Language.Haskell.Exts.Syntax.ModuleName in
--      modifyModuVerse (const modules) >>
--      splitModuleDecls \"Language/Haskell/Modules/Common.hs\" >>
--      mergeModules (map mn [\"Language.Haskell.Modules.Common.WithCurrentDirectory\",
--                            \"Language.Haskell.Modules.Common.ModulePathBase\"])
--                   (mn \"Language.Haskell.Modules.Common\")@
module Language.Haskell.Modules
    ( MonadClean
    , CleanT
    , runCleanT
    , cleanImports
    , splitModule
    , splitModuleDecls
    , mergeModules
    , module Language.Haskell.Modules.Fold
    , module Language.Haskell.Modules.ModuVerse
    , module Language.Haskell.Modules.Params
    , module Language.Haskell.Modules.SourceDirs
    , module Language.Haskell.Modules.Util.QIO
    , findHsModules
    , findHsFiles
    , withCurrentDirectory
    ) where

import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Fold (echo, echo2, foldDecls, foldExports, foldHeader, foldImports, foldModule, ignore, ignore2)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Internal (CleanT, MonadClean, runCleanT)
import Language.Haskell.Modules.Merge (mergeModules)
import Language.Haskell.Modules.ModuVerse (getNames, ModuVerse(..), parseModule, parseModule', putName)
import Language.Haskell.Modules.Params (modifyDryRun, modifyHsFlags, modifyRemoveEmptyImports, modifyTestMode)
import Language.Haskell.Modules.SourceDirs (getDirs, modifyDirs, modulePathBase, putDirs)
import Language.Haskell.Modules.Split (splitModule, splitModuleDecls)
import Language.Haskell.Modules.Util.QIO (noisily, quietly)
import Language.Haskell.Modules.Util.Test (findHsFiles, findHsModules)
