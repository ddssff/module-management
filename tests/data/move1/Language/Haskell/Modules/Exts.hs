-- | Extensions to haskell-src-exts.  Simple utility functions and
-- classes on the haskell-src-exts data structures.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.Modules.Exts where

import qualified Language.Haskell.Exts.Annotated as A -- (Decl, Module(..), ModuleHead(..), ModuleName(..), parseFileWithComments)
import Language.Haskell.Exts.Annotated.Simplify (sModuleName)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S

--type Module = A.Module SrcSpanInfo
--type ModuleHead = A.ModuleHead SrcSpanInfo
type ModulePragma = A.ModulePragma SrcSpanInfo
type ModuleName = A.ModuleName SrcSpanInfo
type WarningText = A.WarningText SrcSpanInfo
type ExportSpec = A.ExportSpec SrcSpanInfo
type ImportDecl = A.ImportDecl SrcSpanInfo
type Decl = A.Decl SrcSpanInfo

class HasModuleName a where
    moduleName :: a -> S.ModuleName

class MayHaveModuleName a where
    moduleNameMaybe :: a -> Maybe S.ModuleName

instance MayHaveModuleName (A.Module SrcSpanInfo) where
    moduleNameMaybe (A.Module _ (Just x) _ _ _) = Just (moduleName x)
    moduleNameMaybe _ = Nothing

--instance HasModuleName a => MayHaveModuleName a where
--    moduleNameMaybe = Just . moduleName

instance HasModuleName (A.ModuleName SrcSpanInfo) where
    moduleName = sModuleName

instance HasModuleName (A.ModuleHead SrcSpanInfo) where
    moduleName (A.ModuleHead _ x _ _) = moduleName x

instance (MayHaveModuleName a, MayHaveModuleName b) => MayHaveModuleName (a, b) where
    moduleNameMaybe (a, b) =
        case (moduleNameMaybe a, moduleNameMaybe b) of
          (Nothing, x) -> x
          (x, Nothing) -> x
          (Just m, Just n) | m == n -> Just m
          (Just _, Just _) -> error "Module name mismatch"
