module Language.Haskell.Modules
    ( foldModule
    , cleanImports
    , splitModule
    , catModules
    , MonadClean
    , runCleanT
    , Params(extensions, hsFlags, removeEmptyImports)
    , modifyParams
    , MonadDryRun(..)
    ) where

import Language.Haskell.Modules.Cat (catModules)
import Language.Haskell.Modules.Fold (foldModule)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Split (splitModule)
import Language.Haskell.Modules.Params (Params(..), MonadClean, runCleanT, modifyParams)
import Language.Haskell.Modules.Util.DryIO (MonadDryRun(..))
