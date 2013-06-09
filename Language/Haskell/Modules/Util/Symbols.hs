{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Util.Symbols
    ( HasSymbols(symbols)
    ) where

import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(..), DeclHead(..), ImportSpec(..), InstHead(..), Match(..), Name(..), QName(..))

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
