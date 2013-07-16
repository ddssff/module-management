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
--    @findHsFiles [\"Language\", \"Tests.hs\", \"Tests\"] >>= runCleanT . cleanImports@
--
-- * Split the module @Language.Haskell.Modules.Common@, and then
--   merge two of the declarations back in:
--
--   @findHsModules [\"Language\", \"Tests.hs\", \"Tests\"] >>= \\ modules -> runCleanT $
--      mapM putModule modules >>
--      splitModuleDecls "Language/Haskell/Modules/Common.hs" >>
--      mergeModules [ModuleName \"Language.Haskell.Modules.Common.WithCurrentDirectory\",
--                    ModuleName \"Language.Haskell.Modules.Common.Internal.ToEq\"]
--                   (ModuleName \"Language.Haskell.Modules.Common\")@
--
-- * Move two declarations from Internal to Common:
--
--   @:m +Language.Haskell.Exts.Syntax
--    findHsModules [\"Language\", \"Tests.hs\", \"Tests\"] >>= \\ modules -> runCleanT $
--      mapM putModule modules >>
--      splitModule (\\ n -> if elem n [Just (Ident \"ModuleResult\"), Just (Ident \"doResult\")]
--                          then ModuleName \"Tmp\"
--                          else ModuleName \"Language.Haskell.Modules.Internal\")
--                  (ModuleName \"Language/Haskell/Modules/Internal.hs\") >>
--      mergeModules [ModuleName \"Language.Haskell.Modules.Common\", ModuleName \"Tmp\"]
--                   (ModuleName \"Language.Haskell.Modules.Common"/)@
module Language.Haskell.Modules
    (
    -- * Entry points
      cleanImports
    , splitModule
    , splitModuleDecls
    , defaultSymbolToModule
    , mergeModules
    -- * Runtime environment
    , MonadClean
    , CleanT
    , runCleanT
    , putModule
    , findModule
    , modifyDryRun
    , modifyHsFlags
    , modifyRemoveEmptyImports
    , modifyExtensions
    , modifyTestMode
    , modifyDirs
    , putDirs
    -- * Progress reporting
    , noisily
    , quietly
    -- * Helper functions
    , modulePathBase
    , findHsModules
    , findHsFiles
    , withCurrentDirectory
    ) where

import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Internal (CleanT, MonadClean, runCleanT)
import Language.Haskell.Modules.Merge (mergeModules)
import Language.Haskell.Modules.ModuVerse (findModule, modifyExtensions, putModule)
import Language.Haskell.Modules.Params (modifyDryRun, modifyHsFlags, modifyRemoveEmptyImports, modifyTestMode)
import Language.Haskell.Modules.SourceDirs (modifyDirs, modulePathBase, SourceDirs(putDirs))
import Language.Haskell.Modules.Split (defaultSymbolToModule, splitModule, splitModuleDecls)
import Language.Haskell.Modules.Util.QIO (noisily, quietly)
import Language.Haskell.Modules.Util.Test (findHsFiles, findHsModules) -- (findHsFiles, findHsModules)
