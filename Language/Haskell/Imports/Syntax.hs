module Language.Haskell.Imports.Syntax
    ( importsSpan
    , replaceImports
    , prettyImports
    , HasSymbol(symbol)
    , nameString
    , renameSpec
    ) where

import Control.Exception (catch, SomeException, throw, try)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B (readFile)
import Data.Char (isSpace)
import Data.Default (def, Default)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.List (groupBy, partition, sortBy, nub)
import Data.Monoid ((<>))
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(..))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Parser (parseModule)
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (mergeSrcSpan, mkSrcSpan, SrcSpan(..))
import Language.Haskell.Exts.Syntax -- (Module(..), ModuleName(..), ImportDecl(..), ExportSpec(..), QName(..), Decl(..), SrcLoc(..))
import Language.Haskell.Imports.SrcLoc
import System.Directory (removeFile, renameFile, getCurrentDirectory, setCurrentDirectory)
import System.IO.Error (isDoesNotExistError)
import Test.HUnit (assertEqual, Test(TestCase, TestList))

-- | Compute the span of the source file which contains the imports by
-- examining the SrcLoc values in the parsed source code and the
-- comments.
importsSpan :: Module -> [Comment] -> SrcSpan
importsSpan (Module _ _ _ _ _ imports@(i : _) (d : _)) comments =
    mkSrcSpan b e
    where
      b = srcLoc i
      -- The imports section ends when the first declaration or
      -- comment following the last import starts
      e = case dropWhile (\ comment -> srcLoc comment <= srcLoc (last imports)) comments of
            (c : _) -> min (srcLoc c) (srcLoc d)
            [] -> srcLoc d
importsSpan _ _ = error "importsSpan"

-- | Compare the old and new import sets and if they differ clip out
-- the imports from the sourceText and insert the new ones.
replaceImports :: [ImportDecl] -> [ImportDecl] -> String -> SrcSpan -> Maybe String
replaceImports oldImports newImports sourceText sp =
    if newPretty /= oldPretty -- the ImportDecls won't match because they have different SrcLoc values
    then let (hd, _, tl) = srcSpanTriple sp sourceText
             -- Instead of inserting this newline we should figure out what was
             -- between the last import and the first declaration, but not sure
             -- how to locate the end of an import.
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

instance HasSymbol Decl where
    symbol (TypeDecl _ name _ _) = symbol name
    symbol (TypeFamDecl _ name _ _) = symbol name
    symbol (DataDecl _ _ _ name _ _ _) = symbol name
    symbol (GDataDecl _ _ _ name _ _ _ _) = symbol name
    symbol (DataFamDecl _ _ name _ _) = symbol name
    symbol x@(TypeInsDecl _ _ _ {-SrcLoc Type Type-}) = error $ "HasSymbol Decl " ++ show x
    symbol x@(DataInsDecl _ _ _ _ _ {-SrcLoc DataOrNew Type [QualConDecl] [Deriving]-}) = error $ "HasSymbol Decl " ++ show x
    symbol x@(GDataInsDecl _ _ _ _ _ _ {-SrcLoc DataOrNew Type (Maybe Kind) [GadtDecl] [Deriving]-}) = error $ "HasSymbol Decl " ++ show x
    symbol (ClassDecl _ _ name _ _ _) = symbol name
    symbol x@(InstDecl _ _ _ _ _ {-SrcLoc Context QName [Type] [InstDecl]-}) = error $ "HasSymbol Decl " ++ show x
    symbol x@(DerivDecl _ _ _ _ {-SrcLoc Context QName [Type]-}) = error $ "HasSymbol Decl " ++ show x
    symbol x@(InfixDecl _ _ _ _ {-SrcLoc Assoc Int [Op]-}) = error $ "HasSymbol Decl " ++ show x
    symbol x@(DefaultDecl _ _ {-SrcLoc [Type]-}) = error $ "HasSymbol Decl " ++ show x
    symbol x@(SpliceDecl _ _ {-SrcLoc Exp-}) = error $ "HasSymbol Decl " ++ show x
    symbol (TypeSig loc names _) = case nub names of
                                        [name] -> symbol name
                                        _ -> error $ "HasSymbol TypeSig: multiple names at " ++ show loc
    symbol x@(FunBind matches) = case nub (map symbol matches) of
                                   [Just name] -> Just name
                                   _ -> error $ "HasSymbol FunBind: multiple matches at " ++ show (srcLoc x)
    symbol x@(PatBind _ _ _ _ _ {-SrcLoc Pat (Maybe Type) Rhs Binds-}) = error $ "HasSymbol Decl " ++ show x
    symbol x@(ForImp _ _ _ _ _ _ {-SrcLoc CallConv Safety String Name Type-}) = error $ "HasSymbol Decl " ++ show x
    symbol x@(ForExp {-SrcLoc CallConv String Name Type-} _ _ _ _ _) = error $ "HasSymbol Decl " ++ show x
    symbol x@(RulePragmaDecl {-SrcLoc [Rule]-} _ _) = error $ "HasSymbol Decl " ++ show x
    symbol x@(DeprPragmaDecl {-SrcLoc [([Name], String)]-} _ _) = error $ "HasSymbol Decl " ++ show x
    symbol x@(WarnPragmaDecl {-SrcLoc [([Name], String)]-} _ _) = error $ "HasSymbol Decl " ++ show x
    symbol x@(InlineSig {-SrcLoc Bool Activation QName-} _ _ _ _) = error $ "HasSymbol Decl " ++ show x
    symbol x@(InlineConlikeSig {-SrcLoc Activation QName-} _ _ _) = error $ "HasSymbol Decl " ++ show x
    symbol x@(SpecSig {-SrcLoc QName [Type]-} _ _ _) = error $ "HasSymbol Decl " ++ show x
    symbol x@(SpecInlineSig {-SrcLoc Bool Activation QName [Type]-} _ _ _ _ _) = error $ "HasSymbol Decl " ++ show x
    symbol x@(InstSig {-SrcLoc Context QName [Type]-} _ _ _ _) = error $ "HasSymbol Decl " ++ show x
    symbol x@(AnnPragma {-SrcLoc Annotation-} _ _) = error $ "HasSymbol Decl " ++ show x

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

nameString :: Name -> String
nameString (Ident s) = s
nameString (Symbol s) = s

renameSpec :: String -> ImportSpec -> ImportSpec
renameSpec s x = mapSpecName (const s) x
-- | Change the symbol name (but not the module path) of an
-- ImportSpec.
mapSpecName :: (String -> String) -> ImportSpec -> ImportSpec
mapSpecName f = foldSpec (IVar . mapName f) (IAbs . mapName f) (IThingAll . mapName f) (\ n cn -> IThingWith (mapName f n) cn)

mapName :: (String -> String) -> Name -> Name
mapName f = foldName (Ident . f) (Symbol . f)

foldSpec :: (Name -> a) -> (Name -> a) -> (Name -> a) -> (Name -> [CName] -> a) -> ImportSpec -> a
foldSpec iVar _ _ _ (IVar n) = iVar n
foldSpec _ iAbs _ _ (IAbs n) = iAbs n
foldSpec _ _ iThingAll _ (IThingAll n) = iThingAll n
foldSpec _ _ _ iThingWith (IThingWith n cn) = iThingWith n cn

foldName :: (String -> a) -> (String -> a) -> Name -> a
foldName ident _ (Ident s) = ident s
foldName _ symbol (Symbol s) = symbol s

-- | Get the symbol name of an ImportSpec.
specName :: ImportSpec -> String
specName = foldSpec (foldName id id) (foldName id id) (foldName id id) (\ n _ -> foldName id id n)
