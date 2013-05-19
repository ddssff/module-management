{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Split
    ( splitModule
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try)
import Control.Monad.Trans (liftIO)
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(ParseOk))
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(..), PPLayout(..), prettyPrintWithMode)
import Language.Haskell.Exts.Syntax -- (Module(..), ModuleName(..), ImportDecl(..), ExportSpec(..), QName(..), Decl(..), SrcLoc(..))
import Language.Haskell.Imports.Params (putDryRun, MonadParams, runParamsT)
import System.FilePath ((</>), (<.>), takeDirectory, takeBaseName)
import Text.Printf (printf)

-- | Split each of the module's declarations into a separate module
-- and minimize each one's imports.  If the declaration was not
-- originally exported add "Internal" to the new module name.  Each of
-- the resulting modules may need to import some of the other split
-- modules, but we don't know which or how to avoid circular imports, so
-- a commented out list is added.
splitModule :: FilePath -> IO ()
splitModule path =
    do source <- liftIO $ try ((,) <$> parseFileWithComments defaultParseMode path <*> readFile path)
       case source of
         Left (e :: SomeException) -> error (path ++ ": " ++ show e)
         Right (ParseOk ((Module loc (ModuleName name) pragmas warn
                                (Just exports) imports decls), comments), text) ->
             -- The symbols that are exported go into modules named
             -- dir.base.symbol, others into dir.base.internal.symbol.
             mapM_ (\ (decl, count) ->
                        case symbol decl of
                          Nothing -> error "splitModule: no symbol"
                          Just name ->
                              do let dir = takeDirectory path
                                     base = takeBaseName path
                                     path' = dir </> base <.> printf "%03d" count <.> "hs"
                                 writeFile path' (prettyPrintWithMode defaultMode (Module loc (ModuleName $ name ++ show count) pragmas warn Nothing imports [decl]))
                   ) (zip decls ([1..] :: [Int]))

class HasSymbol a where
    symbol :: a -> Maybe String

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
    symbol x@(TypeSig _ _ _ {-SrcLoc [Name] Type-}) = error $ "HasSymbol Decl " ++ show x
    symbol x@(FunBind _ {-[Match]-}) = error $ "HasSymbol Decl " ++ show x
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

instance HasSymbol ExportSpec where
    symbol (EVar qname) = symbol qname
    symbol (EAbs qname) = symbol qname
    symbol (EThingAll qname) = symbol qname
    symbol (EThingWith qname _) = symbol qname
    symbol (EModuleContents _) = Nothing

instance HasSymbol QName where
    symbol (Qual _ name) = symbol name
    symbol (UnQual name) = symbol name
    symbol (Special _) = Nothing

instance HasSymbol Name where
    symbol (Ident s) = Just s
    symbol (Symbol s) = Just s
