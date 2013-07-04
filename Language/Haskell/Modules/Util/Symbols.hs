{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.Modules.Util.Symbols
    ( FoldDeclared(foldDeclared)
    , FoldMembers(foldMembers)
    , symbols
    , exports
    , imports
    , tests
    ) where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Set as Set (empty, insert, Set, toList)
import Language.Haskell.Exts.Annotated.Simplify (sName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ClassDecl(..), ConDecl(..), Decl(..), DeclHead(..), Exp(..), ExportSpec(..), FieldDecl(..), GadtDecl(..), ImportSpec(..), InstHead(..), Match(..), Name(..), Pat(..), PatField(..), QName(..), QualConDecl(..), Rhs(..), RPat(..), Type(..))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo(..), SrcSpan(..))
import qualified Language.Haskell.Exts.Syntax as S (CName(..), ExportSpec(..), ImportSpec(..), Name(..), QName(..))
import Language.Haskell.Modules.Util.SrcLoc ()
import Test.HUnit (assertEqual, Test(TestCase, TestList))

-- | Do a fold over the names that are declared in a declaration (not
-- every name that appears, just the ones that the declaration is
-- causing to exist - what's the word for that?.  Reify!)
class FoldDeclared a where
    foldDeclared :: forall r. (Maybe S.Name -> r -> r) -> r -> a -> r

instance FoldDeclared (A.Decl a) where
    foldDeclared f r (A.TypeDecl _ x _t) = foldDeclared f r x  -- type x = ...
    foldDeclared f r (A.TypeFamDecl _ x _k) = foldDeclared f r x -- data family x = ...
    foldDeclared f r (A.DataDecl _ _ _ x _ _) = foldDeclared f r x -- data/newtype _ x = ...
    foldDeclared f r (A.GDataDecl _ _ _ x _ _ _) = foldDeclared f r x
    foldDeclared f r (A.DataFamDecl _ _ x _) = foldDeclared f r x
    foldDeclared f r (A.TypeInsDecl _ _ _) = f Nothing r -- type instance _ = ...
    foldDeclared f r (A.DataInsDecl _ _ _ _ _) = f Nothing r -- data instance _ = ...
    foldDeclared f r (A.GDataInsDecl _ _ _ _ _ _) = f Nothing r -- data or newtype instance, GADT style
    foldDeclared f r (A.ClassDecl _ _ x _ _) = foldDeclared f r x  -- class context => x | fundeps where decls
    foldDeclared f r (A.InstDecl _ _ _ _) = f Nothing r -- type class instance
    foldDeclared f r (A.DerivDecl _ _ x) = foldDeclared f r x
    foldDeclared f r (A.InfixDecl _ _ _ _) = f Nothing r -- fixity
    foldDeclared f r (A.DefaultDecl _ _) = f Nothing r -- default (type1, type2 ...)
    foldDeclared f r (A.SpliceDecl _ _) = f Nothing r  -- template haskell splice declaration
    foldDeclared f r (A.TypeSig _ xs _) = foldl (foldDeclared f) r xs
    foldDeclared f r (A.FunBind _ xs) = foldl (foldDeclared f) r xs
    foldDeclared f r (A.PatBind _ x _ _ _) = foldDeclared f r x
    foldDeclared _f _r (A.ForImp _ _ _ _ _ _) = error "Unimplemented FoldDeclared instance: ForImp"
    foldDeclared _ _r (A.ForExp _ _ _ _ _) = error "Unimplemented FoldDeclared instance: ForExp"
    foldDeclared f r (A.RulePragmaDecl _ _) = f Nothing r
    foldDeclared f r (A.DeprPragmaDecl _ _) = f Nothing r
    foldDeclared f r (A.WarnPragmaDecl _ _) = f Nothing r
    foldDeclared f r (A.InlineSig _ _ _ x) = foldDeclared f r x
    foldDeclared f r (A.InlineConlikeSig _ _ x) = foldDeclared f r x
    foldDeclared f r (A.SpecSig _ x _) = foldDeclared f r x
    foldDeclared f r (A.SpecInlineSig _ _ _ x _) = foldDeclared f r x
    foldDeclared f r (A.InstSig _ _ x) = foldDeclared f r x
    foldDeclared f r (A.AnnPragma _ _) = f Nothing r

instance FoldDeclared (A.DeclHead a) where
    foldDeclared f r (A.DHead _ x _) = foldDeclared f r x
    foldDeclared f r (A.DHInfix _ _ x _) = foldDeclared f r x
    foldDeclared f r (A.DHParen _ x) = foldDeclared f r x
instance FoldDeclared (A.ClassDecl a) where
    foldDeclared f r (A.ClsDecl _ x) = foldDeclared f r x	-- ordinary declaration
    foldDeclared f r (A.ClsDataFam _ _ x _) = foldDeclared f r x	-- declaration of an associated data type
    foldDeclared f r (A.ClsTyFam _ x _) = foldDeclared f r x	-- declaration of an associated type synonym
    foldDeclared _ r (A.ClsTyDef _ _ _) = r -- default choice for an associated type synonym
instance FoldDeclared (A.InstHead a) where
    foldDeclared f r (A.IHead _ x _) = foldDeclared f r x
    foldDeclared f r (A.IHInfix _ _ x _) = foldDeclared f r x
    foldDeclared f r (A.IHParen _ x) = foldDeclared f r x
instance FoldDeclared (A.Match a) where
    foldDeclared f r (A.Match _ x _ _ _) = foldDeclared f r x
    foldDeclared f r (A.InfixMatch _ _ x _ _ _) = foldDeclared f r x
-- Can you declare something with a qualified name?
instance FoldDeclared (A.QName a) where
    foldDeclared f r (A.Qual _ _ x) = foldDeclared f r x
    foldDeclared f r (A.UnQual _ x) = foldDeclared f r x
    foldDeclared _ r (A.Special _ _) = r
instance FoldDeclared (A.Pat a) where
    foldDeclared f r (A.PVar _ x) = foldDeclared f r x	-- variable
    foldDeclared _ r (A.PLit _ _) = r	-- literal constant
    foldDeclared f r (A.PNeg _ x) = foldDeclared f r x	-- negated pattern
    foldDeclared f r (A.PNPlusK _ x _) = foldDeclared f r x	-- n+k pattern
    foldDeclared f r (A.PInfixApp _ p1 _qn p2) = let r' = foldDeclared f r p1 in foldDeclared f r' p2	-- pattern with an infix data constructor
    foldDeclared f r (A.PApp _ _ ps) = foldl (foldDeclared f) r ps	-- data constructor and argument patterns
    foldDeclared f r (A.PTuple _ ps) = foldl (foldDeclared f) r ps	-- tuple pattern
    foldDeclared f r (A.PList _ ps) = foldl (foldDeclared f) r ps	-- list pattern
    foldDeclared f r (A.PParen _ x) = foldDeclared f r x	-- parenthesized pattern
    foldDeclared f r (A.PRec _ _qn fs) = foldl (foldDeclared f) r fs	-- labelled pattern, record style
    foldDeclared f r (A.PAsPat _ x y) = let r' = foldDeclared f r x in foldDeclared f r' y	-- @-pattern
    foldDeclared _ r (A.PWildCard _) = r	-- wildcard pattern: _
    foldDeclared f r (A.PIrrPat _ x) = foldDeclared f r x	-- irrefutable pattern: ~pat
    foldDeclared f r (A.PatTypeSig _ x _) = foldDeclared f r x	-- pattern with type signature
    foldDeclared f r (A.PViewPat _ _ x) = foldDeclared f r x	-- view patterns of the form (exp -> pat)
    foldDeclared f r (A.PRPat _ rps) = foldl (foldDeclared f) r rps	-- regular list pattern
    foldDeclared _f _r (A.PXTag _ _xn _pxs _mp _ps) = error "Unimplemented FoldDeclared instance: PXTag"	-- XML element pattern
    foldDeclared _f _r (A.PXETag _ _xn _pxs _mp) = error "Unimplemented FoldDeclared instance: PXETag"	-- XML singleton element pattern
    foldDeclared _f _r (A.PXPcdata _ _s) = error "Unimplemented FoldDeclared instance: XPcdata"	-- XML PCDATA pattern
    foldDeclared _f _r (A.PXPatTag _ _p) = error "Unimplemented FoldDeclared instance: PXPatTag"	-- XML embedded pattern
    foldDeclared _f _r (A.PXRPats _ _rps) = error "Unimplemented FoldDeclared instance: PXRPats"	-- XML regular list pattern
    foldDeclared _f _r (A.PExplTypeArg _ _n _t) = error "Unimplemented FoldDeclared instance: PExplTypeArg"	-- Explicit generics style type argument e.g. f {| Int |} x = ...
    foldDeclared _ r (A.PQuasiQuote _ _ _) = r	-- quasi quote pattern: [$name| string |]
    foldDeclared f r (A.PBangPat _ x) = foldDeclared f r x	-- strict (bang) pattern: f !x = ...
instance FoldDeclared (A.PatField a) where
    foldDeclared f r (A.PFieldPat _ _n x) = foldDeclared f r x 	-- ordinary label-pattern pair
    foldDeclared f r (A.PFieldPun _ x) = foldDeclared f r x 	-- record field pun
    foldDeclared _ r (A.PFieldWildcard _) = r
instance FoldDeclared (A.RPat a) where
    foldDeclared f r (A.RPOp _ x _) = foldDeclared f r x	-- operator pattern, e.g. pat*
    foldDeclared f r (A.RPEither _ x y) = let r' = foldDeclared f r x in foldDeclared f r' y 	-- choice pattern, e.g. (1 | 2)
    foldDeclared f r (A.RPSeq _ xs) = foldl (foldDeclared f) r xs	-- sequence pattern, e.g. (| 1, 2, 3 |)
    foldDeclared f r (A.RPGuard _ x _) = foldDeclared f r x	-- guarded pattern, e.g. (| p | p < 3 |)
    foldDeclared f r (A.RPCAs _ n x) = let r' = foldDeclared f r n in foldDeclared f r' x 	-- non-linear variable binding, e.g. (foo@:(1 | 2))*
    foldDeclared f r (A.RPAs _ n x) = let r' = foldDeclared f r n in foldDeclared f r' x  	-- linear variable binding, e.g. foo@(1 | 2)
    foldDeclared f r (A.RPParen _ x) = foldDeclared f r x	-- parenthesised pattern, e.g. (2*)
    foldDeclared f r (A.RPPat _ x) = foldDeclared f r x	-- an ordinary pattern

instance FoldDeclared (A.Name l) where
    foldDeclared f r x = f (Just (sName x)) r

-- Something imported can be exported
instance FoldDeclared (A.ImportSpec l) where
    foldDeclared f r (A.IVar _ name) = foldDeclared f r name
    foldDeclared f r (A.IAbs _ name) = foldDeclared f r name
    foldDeclared f r (A.IThingAll _ name) = foldDeclared f r name
    foldDeclared f r (A.IThingWith _ name _) = foldDeclared f r name

instance FoldDeclared (A.ExportSpec l) where
    foldDeclared f r (A.EVar _ name) = foldDeclared f r name
    foldDeclared f r (A.EAbs _ name) = foldDeclared f r name
    foldDeclared f r (A.EThingAll _ name) = foldDeclared f r name
    foldDeclared f r (A.EThingWith _ name _) = foldDeclared f r name
    foldDeclared _ r (A.EModuleContents _ _) = r -- This probably won't work correctly

-- Return the set of symbols appearing in a construct.  Some
-- constructs, such as instance declarations, declare no symbols, in
-- which case Nothing is returned.  Some declare more than one.
symbols :: FoldDeclared a => a -> Set (Maybe S.Name)
symbols = foldDeclared insert empty

members :: FoldMembers a => a -> Set (Maybe S.Name)
members = foldMembers insert empty

justs :: Set (Maybe a) -> [a]
justs = mapMaybe id . toList

exports :: (FoldDeclared a, FoldMembers a) => a -> [S.ExportSpec]
exports x = case (justs (symbols x), justs (members x)) of
              ([n], []) -> [S.EVar (S.UnQual n)]
              ([n], ms) -> [S.EThingWith (S.UnQual n) (sort (map S.VarName ms))]
              ([], []) -> []
              ([], _) -> error "exports: members with no top level name"
              (ns, []) -> map (S.EVar . S.UnQual) ns
              (_ns, _ms) -> error "exports: multiple top level names and member names"

imports :: (FoldDeclared a, FoldMembers a) => a -> [S.ImportSpec]
imports x = case (justs (symbols x), justs (members x)) of
              ([n], []) -> [S.IVar n]
              ([n], ms) -> [S.IThingWith n (sort (map S.VarName ms))]
              ([], []) -> []
              ([], _ms) -> error "exports: members with no top level name"
              (ns, []) -> map S.IVar ns
              (_ns, _ms) -> error "exports: multiple top level names and member names"

-- | Fold over the declared members - e.g. the method names of a class declaration, the constructors of a data declaration.
class FoldMembers a where
    foldMembers :: forall r. (Maybe S.Name -> r -> r) -> r -> a -> r

instance FoldMembers (A.Decl a) where
    foldMembers f r (A.ClassDecl _ _ _ _ mxs) = maybe r (foldl (foldDeclared f) r) mxs  -- class context => x | fundeps where decls
    foldMembers f r (A.DataDecl _ _ _ _ xs _) = foldl (foldDeclared f) r xs -- data/newtype _ x = ...
    foldMembers f r (A.GDataDecl _ _ _ _ _ xs _) = foldl (foldDeclared f) r xs
    foldMembers _ r _ = r

-- The following instances of FoldDeclared are only called by the FoldMembers instances.  Hopefully.
instance FoldDeclared (A.QualConDecl l) where
    foldDeclared f r (A.QualConDecl _l _ _ x) = foldDeclared f r x

-- Constructors and field names
instance FoldDeclared (A.ConDecl l) where
    foldDeclared f r (A.ConDecl _ x _ts) = foldDeclared f r x	-- ordinary data constructor
    foldDeclared f r (A.InfixConDecl _ _t1 x _t2) = foldDeclared f r x	-- infix data constructor
    foldDeclared f r (A.RecDecl _ x fs) = let r' = foldDeclared f r x in foldl (foldDeclared f) r' fs 	-- record constructor

instance FoldDeclared (A.FieldDecl l) where
    foldDeclared f r (A.FieldDecl _ xs _) = foldl (foldDeclared f) r xs

instance FoldDeclared (A.GadtDecl l) where
    foldDeclared f r (A.GadtDecl _ x _) = foldDeclared f r x

tests :: Test
tests = TestList [test1, test2, test3, test4]

test1 :: Test
test1 = TestCase (assertEqual "DefaultDecl" " \ndefault (foo)" (prettyPrint (A.DefaultDecl def [A.TyVar def (A.Ident def "foo")] :: A.Decl SrcSpanInfo)))
test2 :: Test
test2 = TestCase (assertEqual "PatBind" "pvar :: typ = unqualrhs" (prettyPrint (A.PatBind def (A.PVar def (A.Ident def "pvar")) (Just (A.TyVar def (A.Ident def "typ"))) (A.UnGuardedRhs def (A.Var def (A.UnQual def (A.Ident def "unqualrhs")))) Nothing :: A.Decl SrcSpanInfo)))
test3 :: Test
test3 = TestCase (assertEqual "Pat" "pvar" (prettyPrint (A.PVar def (A.Ident def "pvar") :: A.Pat SrcSpanInfo)))
test4 :: Test
test4 = TestCase (assertEqual "Pat" "unqual pvar" (prettyPrint (A.PApp def (A.UnQual def (A.Ident def "unqual")) [A.PVar def (A.Ident def "pvar")] :: A.Pat SrcSpanInfo)))

def :: SrcSpanInfo
def = SrcSpanInfo (SrcSpan "test" 1 1 1 1) []
