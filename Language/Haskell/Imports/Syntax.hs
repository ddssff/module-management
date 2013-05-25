module Language.Haskell.Imports.Syntax
    ( importsSpan
    , replaceImports
    , prettyImports
    , HasSymbol(symbol)
    , renameSpec
    ) where

import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Language.Haskell.Exts (ParseResult(ParseOk))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Parser (parseModule)
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpan, mkSrcSpan)
import Language.Haskell.Exts.Syntax (CName, Decl(..), ExportSpec(..), ImportDecl, ImportSpec(..), Match(..), Module(..), ModuleName(..), Name(..), QName(..))
import Language.Haskell.Imports.SrcLoc (HasSrcLoc(srcLoc), srcSpanTriple, textEndLoc)

-- | Compute the span of the source file which contains the imports by
-- examining the SrcLoc values in the parsed source code and the
-- comments.
importsSpan :: Module -> [Comment] -> String -> SrcSpan
importsSpan (Module _ _ _ _ _ imports@(i : _) ds) comments text =
    mkSrcSpan b e
    where
      b = srcLoc i
      -- The imports section ends when the first declaration or
      -- comment following the last import starts
      e = case dropWhile (\ comment -> srcLoc comment <= srcLoc (last imports)) comments of
            (c : _) -> min (srcLoc c) d
            [] -> d
      d = maybe (textEndLoc text) srcLoc (listToMaybe ds)
importsSpan (Module _ (ModuleName m) _ _ _ _ _) _ _ = error $ "importsSpan " ++ m ++ ": no imports"

-- | Compare the old and new import sets and if they differ clip out
-- the imports from the sourceText and insert the new ones.
replaceImports :: [ImportDecl] -> [ImportDecl] -> String -> SrcSpan -> Maybe String
replaceImports oldImports newImports sourceText sp =
    if newPretty /= oldPretty -- the ImportDecls won't match because they have different SrcLoc values
    then let (hd, _, tl) = srcSpanTriple sp sourceText
             -- Instead of inserting this newline we should figure out what was
             -- between the last import and the first declaration, but not sure
             -- how to locate the end of an import.  (Now I know how, use
             -- foldModule, but I wrote that after this.)
             new = hd <> newPretty <> "\n" <> tl in
         if new /= sourceText then Just new else Nothing
    else Nothing
    where
      oldPretty = prettyImports oldImports
      newPretty = prettyImports newImports

prettyImports :: [ImportDecl] -> String
prettyImports imports =
    munge . prettyPrintWithMode (defaultMode {layout = PPInLine}) $ Module a b c d e imports f
    where
      ParseOk (Module a b c d e _ f) = parseModule ""
      -- Strip off the module declaration line, the leading spaces, and the terminating semicolons
      munge = unlines . map (init . tail . tail) . tail . lines

class HasSymbol a where
    symbol :: a -> Maybe Name

-- Need to finish these
instance HasSymbol Decl where
    symbol (TypeDecl _ name _ _) = symbol name
    symbol (TypeFamDecl _ name _ _) = symbol name
    symbol (DataDecl _ _ _ name _ _ _) = symbol name
    symbol (GDataDecl _ _ _ name _ _ _ _) = symbol name
    symbol (DataFamDecl _ _ name _ _) = symbol name
    symbol x@(TypeInsDecl _ _ _ {-SrcLoc Type Type-}) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(DataInsDecl _ _ _ _ _ {-SrcLoc DataOrNew Type [QualConDecl] [Deriving]-}) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(GDataInsDecl _ _ _ _ _ _ {-SrcLoc DataOrNew Type (Maybe Kind) [GadtDecl] [Deriving]-}) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol (ClassDecl _ _ name _ _ _) = symbol name
    symbol x@(InstDecl _ _ _ _ _ {-SrcLoc Context QName [Type] [InstDecl]-}) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(DerivDecl _ _ _ _ {-SrcLoc Context QName [Type]-}) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(InfixDecl _ _ _ _ {-SrcLoc Assoc Int [Op]-}) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(DefaultDecl _ _ {-SrcLoc [Type]-}) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(SpliceDecl _ _ {-SrcLoc Exp-}) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol (TypeSig loc names _) = case nub names of
                                        [name] -> symbol name
                                        _ -> error $ "HasSymbol TypeSig: multiple names at " ++ show loc
    symbol x@(FunBind matches) = case nub (map symbol matches) of
                                   [Just name] -> Just name
                                   _ -> error $ "HasSymbol FunBind: multiple matches at " ++ show (srcLoc x)
    symbol x@(PatBind _ _ _ _ _ {-SrcLoc Pat (Maybe Type) Rhs Binds-}) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(ForImp _ _ _ _ _ _ {-SrcLoc CallConv Safety String Name Type-}) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(ForExp {-SrcLoc CallConv String Name Type-} _ _ _ _ _) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(RulePragmaDecl {-SrcLoc [Rule]-} _ _) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(DeprPragmaDecl {-SrcLoc [([Name], String)]-} _ _) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(WarnPragmaDecl {-SrcLoc [([Name], String)]-} _ _) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(InlineSig {-SrcLoc Bool Activation QName-} _ _ _ _) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(InlineConlikeSig {-SrcLoc Activation QName-} _ _ _) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(SpecSig {-SrcLoc QName [Type]-} _ _ _) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(SpecInlineSig {-SrcLoc Bool Activation QName [Type]-} _ _ _ _ _) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(InstSig {-SrcLoc Context QName [Type]-} _ _ _ _) = error $ "HasSymbol unimplemented for  " ++ show x
    symbol x@(AnnPragma {-SrcLoc Annotation-} _ _) = error $ "HasSymbol unimplemented for  " ++ show x

instance HasSymbol Match where
    symbol (Match _loc name _ _ _ _) = symbol name

instance HasSymbol ExportSpec where
    symbol (EVar qname) = symbol qname
    symbol (EAbs qname) = symbol qname
    symbol (EThingAll qname) = symbol qname
    symbol (EThingWith qname _) = symbol qname
    symbol (EModuleContents _) = Nothing

instance HasSymbol ImportSpec where
    symbol (IVar name) = symbol name
    symbol (IAbs name) = symbol name
    symbol (IThingAll name) = symbol name
    symbol (IThingWith name _) = symbol name

instance HasSymbol QName where
    symbol (Qual _ name) = symbol name
    symbol (UnQual name) = symbol name
    symbol (Special _) = Nothing

instance HasSymbol Name where
    symbol x = Just x

renameSpec :: Name -> ImportSpec -> ImportSpec
renameSpec s x = mapSpecName (const s) x
-- | Change the symbol name (but not the module path) of an
-- ImportSpec.
mapSpecName :: (Name -> Name) -> ImportSpec -> ImportSpec
mapSpecName f = foldSpec (IVar . f) (IAbs . f) (IThingAll . f) (\ n cn -> IThingWith (f n) cn)

foldSpec :: (Name -> a) -> (Name -> a) -> (Name -> a) -> (Name -> [CName] -> a) -> ImportSpec -> a
foldSpec f _ _ _ (IVar n) = f n
foldSpec _ f _ _ (IAbs n) = f n
foldSpec _ _ f _ (IThingAll n) = f n
foldSpec _ _ _ f (IThingWith n cn) = f n cn
