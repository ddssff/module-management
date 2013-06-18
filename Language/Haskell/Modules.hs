module Language.Haskell.Modules
    ( foldModule
    , cleanImports
    , splitModule
    , catModules
    , MonadClean
    , runCleanT
    , Params(extensions, hsFlags, sourceDirs, moduVerse, removeEmptyImports, testMode)
    , modifyParams
    , MonadDryRun(..)
    , quietly
    , noisily
    , qPutStr
    , qPutStrLn
    , Extension(..)
    , withCurrentDirectory
    , ModuleName(..)
    ) where

import Language.Haskell.Exts.Extension (Extension(..))
import Language.Haskell.Exts.Syntax (ModuleName(ModuleName))
import Language.Haskell.Modules.Cat (catModules)
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Fold (foldModule)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Split (splitModule)
import Language.Haskell.Modules.Params (Params(..), MonadClean, runCleanT, modifyParams)
import Language.Haskell.Modules.Util.DryIO (MonadDryRun(..))
import Language.Haskell.Modules.Util.QIO (quietly, noisily, qPutStr, qPutStrLn)
