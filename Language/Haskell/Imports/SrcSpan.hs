{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.SrcSpan
    ( HasSrcLoc(srcLoc)
    , cutSrcSpan
    ) where

import Data.List (intercalate)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), srcSpanEnd, srcSpanStart)
import Language.Haskell.Exts.Syntax (Decl(..), ImportDecl(..))

class HasSrcLoc x where
    srcLoc :: x -> SrcLoc

instance HasSrcLoc Decl where
    srcLoc (TypeDecl s _ _ _) = s
    srcLoc (TypeFamDecl s _ _ _) = s
    srcLoc (DataDecl s _ _ _ _ _ _) = s
    srcLoc (GDataDecl s _ _ _ _ _ _ _) = s
    srcLoc (DataFamDecl s _ _ _ _) = s
    srcLoc (TypeInsDecl s _ _) = s
    srcLoc (DataInsDecl s _ _ _ _) = s
    srcLoc (GDataInsDecl s _ _ _ _ _) = s
    srcLoc (ClassDecl s _ _ _ _ _) = s
    srcLoc (InstDecl s _ _ _ _) = s
    srcLoc (DerivDecl s _ _ _) = s
    srcLoc (InfixDecl s _ _ _) = s
    srcLoc (DefaultDecl s _) = s
    srcLoc (SpliceDecl s _) = s
    srcLoc (TypeSig s _ _) = s
    srcLoc (FunBind _) = error "srcLoc FunBind"
    srcLoc (PatBind s _ _ _ _) = s
    srcLoc (ForImp s _ _ _ _ _) = s
    srcLoc (ForExp s _ _ _ _) = s
    srcLoc (RulePragmaDecl s _) = s
    srcLoc (DeprPragmaDecl s _) = s
    srcLoc (WarnPragmaDecl s _) = s
    srcLoc (InlineSig s _ _ _) = s
    srcLoc (InlineConlikeSig s _ _) = s
    srcLoc (SpecSig s _ _) = s
    srcLoc (SpecInlineSig s _ _ _ _) = s
    srcLoc (InstSig s _ _ _) = s
    srcLoc (AnnPragma s _) = s

instance HasSrcLoc ImportDecl where
    srcLoc = importLoc

srcSpanStart' :: SrcSpan -> SrcLoc
srcSpanStart' sp = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanStart sp)

srcSpanEnd' :: SrcSpan -> SrcLoc
srcSpanEnd' sp = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanEnd sp)

cutSrcLoc :: SrcLoc -> String -> (String, String)
cutSrcLoc loc s =
    let (h, (t1 : t2)) = splitAt (srcLine loc - 1) (lines s) in
    let (h', t') = splitAt (srcColumn loc - 1) t1 in
    (intercalate "\n" (h ++ [h']), unlines ([t'] ++ t2))

cutSrcSpan :: SrcSpan -> String -> (String, String)
cutSrcSpan sp s =
    let (b, _) = cutSrcLoc (srcSpanStart' sp) s
        (_, e) = cutSrcLoc (srcSpanEnd' sp) s in
    (b, e)
