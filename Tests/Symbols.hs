{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Tests.Symbols
    ( tests
    , test1
    , test2
    , test3
    , test4
    ) where

import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(DefaultDecl, PatBind), Exp(Var), Name(Ident), Pat(PApp, PVar), QName(UnQual), Rhs(UnGuardedRhs), Type(TyVar))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Modules.Util.SrcLoc ()
import Test.HUnit (assertEqual, Test(TestCase, TestList))

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
