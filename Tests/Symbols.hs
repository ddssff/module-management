{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Tests.Symbols
    ( tests
    , test1
    , test2
    , test3
    , test4
    ) where

import Data.Set (fromList)
import Language.Haskell.Exts.Annotated.Syntax as A -- (Decl(DefaultDecl, PatBind), Exp(Var), Name(Ident), Pat(PApp, PVar), QName(UnQual), Rhs(UnGuardedRhs), Type(TyVar))
import Language.Haskell.Exts.Parser (parse, fromParseResult)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpan(..), SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Modules.Util.SrcLoc ()
import Language.Haskell.Modules.Util.Symbols (symbols, members)
import Test.HUnit (assertEqual, Test(TestCase, TestList))

tests :: Test
tests = TestList [test1, test2, test3, test4, test5]

test1 :: Test
test1 = TestCase (assertEqual "DefaultDecl" " \ndefault (foo)" (prettyPrint (A.DefaultDecl def [A.TyVar def (A.Ident def "foo")] :: A.Decl SrcSpanInfo)))
test2 :: Test
test2 = TestCase (assertEqual "PatBind" "pvar :: typ = unqualrhs" (prettyPrint (A.PatBind def (A.PVar def (A.Ident def "pvar")) (Just (A.TyVar def (A.Ident def "typ"))) (A.UnGuardedRhs def (A.Var def (A.UnQual def (A.Ident def "unqualrhs")))) Nothing :: A.Decl SrcSpanInfo)))
test3 :: Test
test3 = TestCase (assertEqual "Pat" "pvar" (prettyPrint (A.PVar def (A.Ident def "pvar") :: A.Pat SrcSpanInfo)))
test4 :: Test
test4 = TestCase (assertEqual "Pat" "unqual pvar" (prettyPrint (A.PApp def (A.UnQual def (A.Ident def "unqual")) [A.PVar def (A.Ident def "pvar")] :: A.Pat SrcSpanInfo)))

test5 :: Test
test5 =
    TestCase (assertEqual "symbols DataDecl" expected (symbols decl, members decl))
    where
      expected = (-- Type name
                  fromList [Just (S.Ident "Paste")],
                  -- Constructor and field accessors
                  fromList [Just (S.Ident "Paste"), Just (S.Ident "pasteMeta"), Just (S.Ident "paste")])
      decl :: A.Decl SrcSpanInfo
      decl = fromParseResult $ parse $ unlines [ "data Paste = Paste"
                                               , "    { pasteMeta :: PasteMeta"
                                               , "    , paste     :: Text"
                                               , "    }"
                                               , "    deriving (Eq, Ord, Read, Show, Data, Typeable)" ]

def :: SrcSpanInfo
def = SrcSpanInfo (SrcSpan "test" 1 1 1 1) []
