{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Common
    ( groupBy'
    , HasSymbols(symbols)
    , voidName
    , mapNames
    , ModuleResult(..)
    , modulePathBase
    ) where

import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(..), DeclHead(..), ExportSpec(..), ExportSpecList(..), ImportDecl(ImportDecl), ImportSpec(..), ImportSpecList, InstHead(..), Match(..), Module, ModuleHead(..), ModuleName(..), ModulePragma(..), Name(..), QName(..), WarningText(..), Type(..))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Control.Applicative ((<$>))
import Control.Exception (bracket, catch, throw)
import Data.Default (def, Default)
import Data.List (groupBy, intercalate, sortBy)
import Language.Haskell.Exts (ParseResult(ParseOk, ParseFailed))
import Language.Haskell.Exts.SrcLoc (srcSpanEnd, srcSpanStart)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..))
import System.Directory (getCurrentDirectory, removeFile, renameFile, setCurrentDirectory)
import System.FilePath ((<.>))
import System.IO.Error (isDoesNotExistError)

-- | Convert a compare function into an (==)
toEq :: Ord a => (a -> a -> Ordering) -> (a -> a -> Bool)
toEq cmp a b =
    case cmp a b of
      EQ -> True
      _ -> False

-- | Combine sortBy and groupBy
groupBy' :: Ord a => (a -> a -> Ordering) -> [a] -> [[a]]
groupBy' cmp xs = groupBy (toEq cmp) $ sortBy cmp xs

class HasSymbols a where
    symbols :: a -> [A.Name ()]

instance HasSymbols (A.Decl a) where
    symbols (A.TypeDecl _ x _) = symbols x
    symbols (A.TypeFamDecl _ x _) = symbols x
    symbols (A.DataDecl _ _ _ x _ _) = symbols x
    symbols (A.GDataDecl _ _ _ x _ _ _) = symbols x
    symbols (A.DataFamDecl _ _ x _) = symbols x
    symbols (A.TypeInsDecl _ _ _) = error "TypeInsDecl"
    symbols (A.DataInsDecl _ _ _ _ _) = error "DataInsDecl"
    symbols (A.GDataInsDecl _ _ _ _ _ _) = error "GDataInsDecl"
    symbols (A.ClassDecl _ _ x _ _) = symbols x
    symbols (A.InstDecl _ _ x _) = symbols x
    symbols (A.DerivDecl _ _ x) = symbols x
    symbols (A.InfixDecl _ _ _ _) = error "InfixDecl"
    symbols (A.DefaultDecl _ _) = error "DefaultDecl"
    symbols (A.SpliceDecl _ _) = error "SpliceDecl"
    symbols (A.TypeSig _ x _) = concatMap symbols x
    symbols (A.FunBind _ matches) = concatMap symbols matches
    symbols (A.PatBind _ _ _ _ _) = error "PatBind"
    symbols (A.ForImp _ _ _ _ _ _) = error "ForImp"
    symbols (A.ForExp _ _ _ _ _) = error "ForExp"
    symbols (A.RulePragmaDecl _ _) = error "RulePragmaDecl"
    symbols (A.DeprPragmaDecl _ _) = error "DeprPragmaDecl"
    symbols (A.WarnPragmaDecl _ _) = error "WarnPragmaDecl"
    symbols (A.InlineSig _ _ _ x) = symbols x
    symbols (A.InlineConlikeSig _ _ x) = symbols x
    symbols (A.SpecSig _ x _) = symbols x
    symbols (A.SpecInlineSig _ _ _ x _) = symbols x
    symbols (A.InstSig _ _ x) = symbols x
    symbols (A.AnnPragma _ _) = error "AnnPragma"

instance HasSymbols (A.DeclHead a) where
    symbols (A.DHead _ x _) = symbols x
    symbols (A.DHInfix _ _ x _) = symbols x
    symbols (A.DHParen _ x) = symbols x

instance HasSymbols (A.InstHead a) where
    symbols (A.IHead _ x _) = symbols x
    symbols (A.IHInfix _ _ x _) = symbols x
    symbols (A.IHParen _ x) = symbols x

instance HasSymbols (A.QName a) where
    symbols (A.Qual _ _ x) = symbols x
    symbols (A.UnQual _ x) = symbols x
    symbols (A.Special _ _) = error "Special"

instance HasSymbols (A.Name a) where
    symbols (A.Ident _ x) = [A.Ident () x]
    symbols (A.Symbol _ x) = [A.Symbol () x]

instance HasSymbols (A.Match a) where
    symbols (A.Match _ x _ _ _) = symbols x
    symbols (A.InfixMatch _ _ x _ _ _) = symbols x

instance HasSymbols (A.ImportSpec a) where
    symbols (A.IVar _ name) = symbols name
    symbols (A.IAbs _ name) = symbols name
    symbols (A.IThingAll _ name) = symbols name
    symbols (A.IThingWith _ name _) = symbols name

voidName :: A.Name a -> A.Name ()
voidName (A.Ident _ x) = A.Ident () x
voidName (A.Symbol _ x) = A.Symbol () x

mapNames :: Default a => [A.Name ()] -> [A.Name a]
mapNames [] = []
mapNames (A.Ident () x : more) = A.Ident def x : mapNames more
mapNames (A.Symbol () x : more) = A.Symbol def x : mapNames more

data ModuleResult
    = Unchanged S.ModuleName
    | Removed S.ModuleName
    | Modified S.ModuleName String
    deriving (Show, Eq, Ord)

-- | Construct the base of a module path.
modulePathBase :: S.ModuleName -> FilePath
modulePathBase (S.ModuleName name) =
    map f name <.> "hs"
    where
      f '.' = '/'
      f c = c
