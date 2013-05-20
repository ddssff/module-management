{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Split
    ( splitModule
    , tests
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try)
import Control.Monad.Trans (liftIO)
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Default (def)
import Data.Function (on)
import Data.List as List (nub, find, isSuffixOf, filter)
import Data.Map as Map (Map, insertWith, empty, toList, mapWithKey, elems, delete, fromList, lookup, filter)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>))
import Data.Set as Set (Set, toList, member, null, intersection, fromList)
import Data.Set.Extra (gFind)
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(ParseOk))
import Language.Haskell.Exts.Comments (Comment)
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(..), PPLayout(..), prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpan)
import Language.Haskell.Exts.Syntax -- (Module(..), ModuleName(..), ImportDecl(..), ExportSpec(..), QName(..), Decl(..), SrcLoc(..))
import Language.Haskell.Imports.Common (groupBy')
import Language.Haskell.Imports.Fold (foldModule)
import Language.Haskell.Imports.Params (putDryRun, MonadParams, runParamsT)
import Language.Haskell.Imports.SrcLoc (HasSrcLoc(srcLoc))
import Prelude hiding (head)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeDirectory, takeBaseName, dropExtension)
import Test.HUnit (assertEqual, Test(TestCase, TestList))
import Text.Printf (printf)

-- Should be QName -> Name -> QName or something
subModuleName :: ModuleName -> Name -> ModuleName
subModuleName (ModuleName moduleName) name =
    ModuleName (case name of
                  Symbol name -> moduleName <.> f name
                  Ident name -> moduleName <.> f name)
    where
      f x =
          case List.filter isAlphaNum x of
            [] -> "OtherSymbols"
            (c : s) -> if isAlpha c then toUpper c : s else error $ "symbolToModuleName: must begin with a letter: " ++ show x

-- | Split each of a module's declarations into a new module.  The
-- resulting modules may need to import some of the other split
-- modules, but we don't know which or how to avoid circular imports,
-- so a commented out list of imports is added.
splitModule :: FilePath -> IO ()
splitModule path =
    do source <- liftIO $ try ((,) <$> parseFileWithComments defaultParseMode path <*> readFile path)
       case source of
         Left (e :: SomeException) -> error (path ++ ": " ++ show e)
         Right (ParseOk (m@(Module loc moduleName pragmas warn (Just exports) imports decls), comments), text) ->
             case decls of
               [] -> error "splitModule: module only no declarations"
               [x] -> error "splitModule: module only has one declaration"
               xs -> do let -- Build a (new module name, declaration list) map
                            declPairs :: Map ModuleName [(Decl, String)]
                            declPairs =
                                foldl ins Map.empty (foldModule
                                                           (\ _ _ _ _ r -> r)
                                                           (\ _ _ _ _ r -> r)
                                                           (\ x pre s sp r -> (symbol x, (x, maybe "" fst pre <> s)) : r)
                                                           (\ _ _ r -> r)
                                                           m comments text [])
                            ins :: Map ModuleName [a] -> (Maybe Name, a) -> Map ModuleName [a]
                            ins mp (Just name, x) = insertWith (++) (subModuleName moduleName name) [x] mp
                            ins mp (Nothing, x) = error "splitModule: no symbol"


                            -- Map from the symbol we will declare to the module name that will
                            -- contain it.  If a symbol used in a declaration is mentioned in
                            -- this map we need to import the corresponding module.
                            declSymbols :: Map Name ModuleName
                            declSymbols = Map.fromList (concatMap (\ (moduleName, declPairs) -> map (\ (decl, _) -> (fromJust (symbol decl), moduleName)) declPairs) (Map.toList declPairs))

                            allNewImports :: Map ModuleName ImportDecl
                            allNewImports = mapWithKey toImportDecl declPairs
                            -- allNewImports = mapWithKey  declPairs
                            -- Grab any comment before the module header
                            oldHeader = foldModule (\ _ pre _ _ r -> r <> maybe "" fst pre) (\ _ _ _ _ r -> r) (\ _ _ _ _ r -> r) (\ s _ r -> r <> s) m comments text ""
                            -- Grab the old imports
                            oldImports = foldModule (\ _ _ _ _ r -> r) (\ _ pre s _ r -> r <> maybe "" fst pre <> s) (\ _ _ _ _ r -> r) (\ _ _ r -> r) m comments text ""
                            -- Build the new modules
                            newModules :: Map ModuleName String
                            newModules = mapWithKey (\ mod decls ->
                                                         let newExports = catMaybes (map toExportSpec decls) in
                                                         -- In this module, we need to import any module that declares a symbol
                                                         -- referenced here.
                                                         let referenced = gFind decls :: Set Name in
                                                         let newImports = mapWithKey toImportDecl (Map.delete mod
                                                                                                   (Map.filter (\ pairs ->
                                                                                                                    let declared = Set.fromList (map (fromJust . symbol . fst) pairs) in
                                                                                                                    not (Set.null (Set.intersection declared referenced))) declPairs)) in
                                                         oldHeader <>
                                                         prettyPrintWithMode defaultMode (Module
                                                                                              loc
                                                                                              mod
                                                                                              pragmas
                                                                                              warn
                                                                                              (Just newExports)
                                                                                              (elems newImports)
                                                                                              []) <>
                                                         "\n\n" <>
                                                         oldImports <>
                                                         concatMap snd decls) declPairs
                            -- Replace the body of the old module with imports
                            newReExporter = oldHeader <>
                                            prettyPrintWithMode defaultMode (Module loc (subModuleName moduleName (Symbol "ReExporter")) pragmas warn (Just exports) (elems allNewImports) [])
                        -- Create a subdirectory named after the old module
                        createDirectoryIfMissing True (dropExtension path)
                        -- Write the new modules
                        mapM_ (\ (ModuleName mod, text) -> writeFile (map (\ c -> case c of '.' -> '/'; c -> c) mod <.> "hs") text) (Map.toList newModules)
                        -- Write the re-exporter
                        writeFile (dropExtension path <.> "ReExporter" <.> "hs") newReExporter
{-
                        -- mapM_ (putStrLn . show) (Map.toList modules)
                        mapM_ (\ (name, decls) ->
                                   -- Build a module to contain just the decls in this list.  It will have
                                   -- name appended to its module name, the same imports plus a
                                   let exports' = [] in
                                   writeFile
                                     (dropExtension path </> name <.> "hs")
                                     (prettyPrintWithMode defaultMode (Module loc (ModuleName (moduleName <.> name)) pragmas warn (Just exports') imports []) <> "\n" + concat decls))
                              (Map.toList modules)
-}
    where
      declf :: Decl -> Maybe (String, SrcSpan) -> String -> SrcSpan -> [(Maybe Name, String)] -> [(Maybe Name, String)]
      declf x pre s sp r = (symbol x, maybe "" fst pre <> s) : r
                   -- The symbols that are exported go into modules named
                   -- dir.base.symbol, others into dir.base.internal.symbol.
{-
                   let (public, private) = partition exported decls in
                   mapM_ (\ (decl, count) ->
                              case symbol decl of
                                Nothing -> error "splitModule: no symbol"
                                Just name ->
                                    do let dir = takeDirectory path
                                           base = takeBaseName path
                                           path' = dir </> base <.> printf "%03d" count <.> "hs"
                                       writeFile path' (prettyPrintWithMode defaultMode (Module loc (ModuleName $ name ++ show count) pragmas warn Nothing imports [decl]))
                         ) (zip decls ([1..] :: [Int]))
-}

exported :: Module -> String -> Bool
exported (Module _ _ _ _ exports _ _) s =
    maybe True (exported' s) exports
    where
      exported' :: String -> [ExportSpec] -> Bool
      exported' s exports =
          any (== (Just s)) (map (fmap nameString . symbol) exports)

toImportDecl :: ModuleName -> [(Decl, String)] -> ImportDecl
toImportDecl mod decls =
    ImportDecl {importLoc = def,
                importModule = mod,
                importQualified = False,
                importSrc = False,
                importPkg = Nothing,
                importAs = Nothing,
                importSpecs = Just (False, nub (map toImportSpec decls))}

toImportSpec :: (Decl, String) -> ImportSpec
toImportSpec = IVar . fromJust . symbol . fst

mkImport :: ImportSpec -> ModuleName -> ImportDecl
mkImport sym mod =
    ImportDecl {importLoc = def,
                importModule = mod,
                importQualified = False,
                importSrc = False,
                importPkg = Nothing,
                importAs = Nothing,
                importSpecs = Just (False, [sym])}

toExportSpec :: (Decl, String) -> Maybe ExportSpec
toExportSpec (x, _) =
    case symbol x of
      Just name -> Just (EVar (UnQual name))
      Just name -> Just (EVar (UnQual name))
      Nothing -> Nothing

nameString :: Name -> String
nameString (Ident s) = s
nameString (Symbol s) = s

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
    symbol x@(TypeSig loc names _) = case nub names of
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

instance HasSymbol QName where
    symbol (Qual _ name) = symbol name
    symbol (UnQual name) = symbol name
    symbol (Special _) = Nothing

instance HasSymbol Name where
    symbol x = Just x

tests :: Test
tests = TestList [test1]

test1 :: Test
test1 =
    TestCase
      (setCurrentDirectory "testdata" >>
       splitModule "Debian/Repo/Package.hs" >>= \ result ->
       assertEqual
         "splitModule"
         ()
         result)
