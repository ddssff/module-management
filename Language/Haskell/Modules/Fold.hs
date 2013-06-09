{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Fold
    ( foldModule
    , test1
    ) where

import Control.Exception (SomeException, try)
import Data.Default (Default(def))
import Language.Haskell.Exts.Annotated (defaultParseMode, parseFileWithComments, ParseResult(..))
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.SrcLoc (SrcLoc, SrcSpanInfo)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(..), DeclHead(..), ExportSpec(..), ExportSpecList(..), ImportDecl(ImportDecl), ImportSpec(..), ImportSpecList, InstHead(..), Match(..), Module, ModuleHead(..), ModuleName(..), ModulePragma(..), Name(..), QName(..), WarningText(..), Type(..))
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ExportSpecList(ExportSpecList), Module(Module, XmlHybrid, XmlPage), ModuleHead(ModuleHead))
import Language.Haskell.Modules.Util.IO (withCurrentDirectory)
import Language.Haskell.Modules.Util.SrcLoc (HasEndLoc(..), HasSrcLoc(..), HasSrcSpan(..), srcPairText, untabify)
import Test.HUnit (assertEqual, Test(TestCase, TestLabel))

type Module = A.Module SrcSpanInfo
type ModuleHead = A.ModuleHead SrcSpanInfo
type ModulePragma = A.ModulePragma SrcSpanInfo
type ModuleName = A.ModuleName SrcSpanInfo
type WarningText = A.WarningText SrcSpanInfo
type ExportSpecList = A.ExportSpecList SrcSpanInfo
type ExportSpec = A.ExportSpec SrcSpanInfo
type ImportDecl = A.ImportDecl SrcSpanInfo
type ImportSpecList = A.ImportSpecList SrcSpanInfo
type ImportSpec = A.ImportSpec SrcSpanInfo
type Decl = A.Decl SrcSpanInfo
type QName = A.QName SrcSpanInfo
type Name = A.Name SrcSpanInfo
type Type = A.Type SrcSpanInfo

-- | Given the result of parseModuleWithComments and the original
-- module text, this does a fold over the parsed module contents,
-- calling the seven argument functions in order.  Each function is
-- passed the AST value, the text of the space and comments leading up
-- to the element, and the text for the element.  Note that not
-- everything passed to the "pre" argument of the functions will be
-- comments and space - for example, the "module" keyword will be
-- passed in the pre argument to the ModuleName function.
foldModule :: forall r.
              (ModulePragma -> String -> String -> r -> r)
           -> (ModuleName -> String -> String -> r -> r)
           -> (WarningText -> String -> String -> r -> r)
           -> (ExportSpec -> String -> String -> r -> r)
           -> (ImportDecl -> String -> String -> r -> r)
           -> (Decl -> String -> String -> r -> r)
           -> (String -> r -> r)
           -> Module -> String -> r -> r
foldModule _ _ _ _ _ _ _ (A.XmlPage _ _ _ _ _ _ _) _ _ = error "XmlPage: unsupported"
foldModule _ _ _ _ _ _ _ (A.XmlHybrid _ _ _ _ _ _ _ _ _) _ _ = error "XmlHybrid: unsupported"
foldModule pragmaf namef warnf exportf importf declf tailf (A.Module _ mh ps is ds) text0 r0 =
    let text = untabify text0
        (r1, l1) = doList text pragmaf ps (r0, def)
        (r2, l2) = case mh of
                     Nothing -> (r1, l1)
                     Just h -> foldModuleHead namef warnf exportf h text (r1, l1)
        (r3, l3) = doList text importf is (r2, l2)
        (r4, l4) = doList text declf ds (r3, l3)
        e = endLoc text
        r5 = if e == l4 then r4 else tailf (srcPairText l4 e text) r4 in
    r5

foldModuleHead :: forall r.
                  (ModuleName -> String -> String -> r -> r)
               -> (WarningText -> String -> String -> r -> r)
               -> (ExportSpec -> String -> String -> r -> r)
               -> ModuleHead -> String -> (r, SrcLoc) -> (r, SrcLoc)
foldModuleHead namef warnf exportf (A.ModuleHead _ n mw me) text0 (r0, l0) =
    let (r1, l1) = doItem text namef n (r0, l0)
        (r2, l2) = case mw of
                     Nothing -> (r1, l1)
                     Just w -> doItem text warnf w (r1, l1)
        (r3, l3) = case me of
                     Nothing -> (r2, l2)
                     Just (A.ExportSpecList _ es) -> doList text exportf es (r2, l2) in
    (r3, l3)
    where
      text = untabify text0

doList :: HasSrcSpan a => String -> (a -> String -> String -> r -> r) -> [a] -> (r, SrcLoc) -> (r, SrcLoc)
doList _ _ [] (r, l) = (r, l)
doList text f (x : xs) (r, l) = doList text f xs (doItem text f x (r, l))

doItem :: HasSrcSpan a => String -> (a -> String -> String -> r -> r) -> a -> (r, SrcLoc) -> (r, SrcLoc)
doItem text f x (r, l) =
    (r', l')
    where
      l' = endLoc (srcSpan x)
      r' = f x (srcPairText l (srcLoc (srcSpan x)) text) (srcPairText (srcLoc (srcSpan x)) l' text) r

test1 :: Test
test1 =
    TestLabel "test1" $ TestCase
    (withTestData test >>= \ (output, original) ->
     assertEqual
     "echo"
     original
     output)
    where
      test :: Module -> [Comment] -> String -> (String, String)
      test m _ text =
          (foldModule pragmaf namef warningf exportf importf declf tailf m text [], text)
          where
            pragmaf :: ModulePragma -> String -> String -> String -> String
            pragmaf _x pre s r = r ++ pre ++ s
            -- pragmaf _x pre s r = r ++ "pragma: " ++ show s ++ "\n"
            namef :: ModuleName -> String -> String -> String -> String
            namef _x pre s r = r ++ pre ++ s
            -- namef _x pre s r = r ++ "name: " ++ show s ++ "\n"
            warningf :: WarningText -> String -> String -> String -> String
            warningf _x pre s r = r ++ pre ++ s
            -- warningf _x pre s r = r ++ "warning: " ++ show s ++ "\n"
            exportf :: ExportSpec -> String -> String -> String -> String
            exportf _x pre s r = r ++ pre ++ s
            -- exportf _x pre s r = r ++ "export: " ++ show s ++ "\n"
            importf :: ImportDecl -> String -> String -> String -> String
            importf _x pre s r = r ++ pre ++ s
            -- importf _x pre s r = r ++ "import: " ++ show s ++ "\n"
            declf :: Decl -> String -> String -> String -> String
            declf _x pre s r = r ++ pre ++ s
            -- declf _x pre s r = r ++ "decl: " ++ show s ++ "\n"
            tailf :: String -> String -> String
            tailf s r = r ++ s
            -- spacef "" r = r
            -- spacef s r = r ++ "space: " ++ show s ++ "\n"

withTestData :: (Module -> [Comment] -> String -> r) -> IO r
withTestData f = withCurrentDirectory "testdata/original" $
    do let path = "Debian/Repo/AptCache.hs"
       text <- try (readFile path)
       source <- try (parseFileWithComments defaultParseMode path)
       case (text, source) of
         (Right text', Right (ParseOk (m, comments))) ->
             return $ f m comments (untabify text')
         (Right _, Right _) -> error "parse failure"
         (Left (e :: SomeException), _) -> error $ "failure: " ++ show e
         (_, Left (e :: SomeException)) -> error $ "failure: " ++ show e
