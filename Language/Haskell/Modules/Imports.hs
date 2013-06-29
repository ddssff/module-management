{-# LANGUAGE PackageImports, ScopedTypeVariables, StandaloneDeriving, TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Imports
    ( cleanImports
    -- , cleanBuildImports
    , tests
    ) where

import Control.Applicative ((<$>))
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (bracket, catch, throw)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Default (def, Default)
import Data.Function (on)
import Data.List (find, groupBy, intercalate, nub, nubBy, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (empty, member, Set, singleton, toList, union, unions)
import Language.Haskell.Exts.Annotated (ParseResult(..))
import Language.Haskell.Exts.Annotated.Simplify as S (sImportDecl, sImportSpec, sModuleName, sName)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl(DerivDecl), ImportDecl(..), ImportSpec(..), ImportSpecList(ImportSpecList), InstHead(..), Module(..), ModuleHead(ModuleHead), ModuleName(ModuleName), QName(..), Type(..))
import Language.Haskell.Exts.Extension (Extension(PackageImports, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances))
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ImportDecl(importLoc, importModule, importSpecs), ModuleName(..), Name(..))
import Language.Haskell.Modules.Common (modulePathBase, withCurrentDirectory)
import Language.Haskell.Modules.Fold (ModuleInfo, foldDecls, foldExports, foldHeader, foldImports)
import Language.Haskell.Modules.Internal (getParams, markForDelete, modifyParams, ModuleResult(..), MonadClean, Params(..), parseFile, parseFileWithComments, runMonadClean, scratchDir)
import Language.Haskell.Modules.Params (modifyTestMode)
import Language.Haskell.Modules.Util.DryIO (replaceFile, tildeBackup)
import Language.Haskell.Modules.Util.QIO (qPutStrLn, quietly)
import Language.Haskell.Modules.Util.Symbols (symbols)
import System.Cmd (system)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((<.>), (</>))
import System.Process (readProcessWithExitCode, showCommandForUser)
import Test.HUnit (assertEqual, Test(..))

{-
-- | This is designed to be called from the postConf script of your
-- Setup file, it cleans up the imports of all the source files in the
-- package.

cleanBuildImports :: LocalBuildInfo -> IO ()
cleanBuildImports lbi =
    mapM (toFilePath srcDirs) (maybe [] exposedModules (library (localPkgDescr lbi))) >>= \ libPaths ->
    runMonadClean (Distribution.Simple.LocalBuildInfo.scratchDir lbi) $ mapM_ clean (libPaths ++ exePaths)
    where
      clean path = cleanImports path >>= liftIO . putStrLn . either show (\ text -> path ++ ": " ++ maybe "no changes" (\ _ -> " updated") text)
      exePaths = map modulePath (executables (localPkgDescr lbi))
      srcDirs = case (maybe [] hsSourceDirs . fmap libBuildInfo . library . localPkgDescr $ lbi) of
                  [] -> ["."]
                  xs -> xs
      toFilePath :: [FilePath] -> D.ModuleName -> IO FilePath
      toFilePath [] m = error $ "Missing module: " ++ intercalate "." (D.components m)
      toFilePath (dir : dirs) m =
          let path = (dir </> intercalate "/" (D.components m) <.> "hs") in
          doesFileExist path >>= \ exists ->
          if exists then return path else toFilePath dirs m
-}

-- | Clean up the imports of a source file.
cleanImports :: MonadClean m => FilePath -> m ModuleResult
cleanImports path =
    do text <- liftIO $ readFile path
       source <- parseFileWithComments path
       case source of
         ParseOk (m@(A.Module _ h _ imports _decls), comments) ->
             do let name = case h of
                             Just (A.ModuleHead _ x _ _) -> sModuleName x
                             _ -> S.ModuleName "Main"
                    hiddenImports = filter isHiddenImport imports
                dumpImports path >> checkImports path name (m, text, comments) hiddenImports
         ParseOk (A.XmlPage {}, _) -> error "cleanImports: XmlPage"
         ParseOk (A.XmlHybrid {}, _) -> error "cleanImports: XmlHybrid"
         ParseFailed _loc msg -> error ("cleanImports: - parse of " ++ path ++ " failed: " ++ msg)
    where
      isHiddenImport (A.ImportDecl {A.importSpecs = Just (A.ImportSpecList _ True _)}) = True
      isHiddenImport _ = False

-- | Run ghc with -ddump-minimal-imports and capture the resulting .imports file.
dumpImports :: MonadClean m => FilePath -> m ()
dumpImports path =
    do scratch <- scratchDir <$> getParams
       liftIO $ createDirectoryIfMissing True scratch
       let cmd = "ghc"
       args <- hsFlags <$> getParams
       dirs <- sourceDirs <$> getParams
       exts <- extensions <$> getParams
       let args' = args ++ ["--make", "-c", "-ddump-minimal-imports", "-outputdir", scratch, "-i" ++ intercalate ":" dirs, path] ++ map (("-X" ++) . show) exts
       (code, _out, err) <- liftIO $ readProcessWithExitCode cmd args' ""
       case code of
         ExitSuccess -> quietly (qPutStrLn (showCommandForUser cmd args' ++ " -> Ok")) >> return ()
         ExitFailure _ -> error ("dumpImports: compile failed\n " ++ showCommandForUser cmd args' ++ " ->\n" ++ err)

-- | Parse the import list generated by GHC, parse the original source
-- file, and if all goes well insert the new imports into the old
-- source file.  We also need to modify the imports of any names
-- that are types that appear in standalone instance derivations so
-- their members are imported too.
checkImports :: MonadClean m => FilePath -> S.ModuleName -> ModuleInfo -> [A.ImportDecl SrcSpanInfo] -> m ModuleResult
checkImports path name@(S.ModuleName name') m extraImports =
    do let importsPath = name' <.> ".imports"
       markForDelete importsPath
       result <-
           bracket (getParams >>= return . extensions)
                   (\ saved -> modifyParams (\ p -> p {extensions = saved}))
                   (\ saved -> modifyParams (\ p -> p {extensions = PackageImports : saved}) >>
                               parseFile importsPath `catch` (\ (e :: IOError) -> liftIO (getCurrentDirectory >>= \ here -> throw . userError $ here ++ ": " ++ show e)))
       case result of
         ParseOk newImports -> updateSource path m newImports name extraImports
         _ -> error ("checkImports: parse of " ++ importsPath ++ " failed - " ++ show result)

-- | If all the parsing went well and the new imports differ from the
-- old, update the source file with the new imports.
updateSource :: MonadClean m => FilePath -> ModuleInfo -> A.Module SrcSpanInfo -> S.ModuleName -> [A.ImportDecl SrcSpanInfo] -> m ModuleResult
updateSource path m@(A.Module _ _ _ oldImports _, _, _) (A.Module _ _ _ newImports _) name extraImports =
    do remove <- removeEmptyImports <$> getParams
       maybe (qPutStrLn ("cleanImports: no changes to " ++ path) >> return (Unchanged name))
             (\ text' ->
                  qPutStrLn ("cleanImports: modifying " ++ path) >>
                  replaceFile tildeBackup path text' >>
                  return (Modified name text'))
             (replaceImports (fixNewImports remove m oldImports (newImports ++ extraImports)) m)
updateSource _ _ _ _ _ = error "updateSource"

-- | Compare the old and new import sets and if they differ clip out
-- the imports from the sourceText and insert the new ones.
replaceImports :: [A.ImportDecl SrcSpanInfo] -> ModuleInfo -> Maybe String
replaceImports newImports m =
    let oldPretty = foldImports (\ _ pref s suff r -> r <> pref <> s <> suff) m ""
        -- Surround newPretty with the same prefix and suffix as oldPretty
        newPretty = fromMaybe "" (foldImports (\ _ pref _ _ r -> maybe (Just pref) Just r) m Nothing) <>
                    intercalate "\n" (map (prettyPrintWithMode (defaultMode {layout = PPInLine})) newImports) <>
                    foldImports (\ _ _ _ suff _ -> suff) m "" in
    if oldPretty == newPretty
    then Nothing
    else Just (foldHeader (\ s r -> r <> s) (\ _ pref s suff r -> r <> pref <> s <> suff) (\ _ pref s suff r -> r <> pref <> s <> suff) (\ _ pref s suff r -> r <> pref <> s <> suff) m "" ++
               foldExports (\ s r -> r <> s) (\ _ pref s suff r -> r <> pref <> s <> suff) (\ s r -> r <> s) m "" ++
               newPretty <>
               foldDecls  (\ _ pref s suff r -> r <> pref <> s <> suff) (\ r s -> s <> r) m "")

-- | Final touch-ups - sort and merge similar imports.
fixNewImports :: Bool         -- ^ If true, imports that turn into empty lists will be removed
              -> ModuleInfo
              -> [A.ImportDecl SrcSpanInfo]
              -> [A.ImportDecl SrcSpanInfo]
              -> [A.ImportDecl SrcSpanInfo]
fixNewImports remove m oldImports imports =
    filter importPred $ map expandSDTypes $ map mergeDecls $ groupBy (\ a b -> importMergable a b == EQ) $ sortBy importMergable imports
    where
      -- mergeDecls :: [ImportDecl] -> ImportDecl
      mergeDecls [] = error "mergeDecls"
      mergeDecls xs@(x : _) = x {A.importSpecs = mergeSpecLists (catMaybes (map A.importSpecs xs))}
          where
            -- Merge a list of specs for the same module
            -- mergeSpecLists :: [ImportSpecList] -> Maybe ImportSpecList
            mergeSpecLists (A.ImportSpecList loc flag specs : ys) =
                Just (A.ImportSpecList loc flag (mergeSpecs (sortBy compareSpecs (nub (concat (specs : map (\ (A.ImportSpecList _ _ specs') -> specs') ys))))))
            mergeSpecLists [] = error "mergeSpecLists"
            -- unimplemented, should merge Foo and Foo(..) into Foo(..), and the like
            mergeSpecs ys = nubBy equalSpecs ys
      expandSDTypes :: A.ImportDecl SrcSpanInfo -> A.ImportDecl SrcSpanInfo
      expandSDTypes i@(A.ImportDecl {A.importSpecs = Just (A.ImportSpecList l f specs)}) =
          i {A.importSpecs = Just (A.ImportSpecList l f (map (expandSpec i) specs))}
      expandSDTypes i = i
      expandSpec i s =
          if not (A.importQualified i) && member (Nothing, sName n) sdTypes ||
             maybe False (\ mn -> (member (Just (sModuleName mn), sName n) sdTypes)) (A.importAs i) ||
             member (Just (sModuleName (A.importModule i)), sName n) sdTypes
          then s'
          else s
          where
            n = case s of
                  (A.IVar _ x) -> x
                  (A.IAbs _ x) -> x
                  (A.IThingAll _ x) -> x
                  (A.IThingWith _ x _) -> x
            s' = case s of
                  (A.IVar l x) -> A.IThingAll l x
                  (A.IAbs l x) -> A.IThingAll l x
                  (A.IThingWith l x _) -> A.IThingAll l x
                  (A.IThingAll _ _) -> s

      -- Eliminate imports that became empty
      -- importPred :: ImportDecl -> Bool
      importPred (A.ImportDecl _ mn _ _ _ _ (Just (A.ImportSpecList _ _ []))) =
          not remove || maybe False (isEmptyImport . A.importSpecs) (find ((== (unModuleName mn)) . unModuleName . A.importModule) oldImports)
          where
            isEmptyImport (Just (A.ImportSpecList _ _ [])) = True
            isEmptyImport _ = False
      importPred _ = True

      sdTypes :: Set (Maybe S.ModuleName, S.Name)
      sdTypes = standaloneDerivingTypes m

standaloneDerivingTypes :: ModuleInfo -> Set (Maybe S.ModuleName, S.Name)
standaloneDerivingTypes (A.XmlPage _ _ _ _ _ _ _, _, _) = error "standaloneDerivingTypes A.XmlPage"
standaloneDerivingTypes (A.XmlHybrid _ _ _ _ _ _ _ _ _, _, _) = error "standaloneDerivingTypes A.XmlHybrid"
standaloneDerivingTypes (A.Module _ _ _ _ decls, _, _) =
    unions (map derivDeclTypes decls)
    where
      -- derivDeclTypes :: Decl -> Set (Maybe S.ModuleName, S.Name)
      derivDeclTypes (A.DerivDecl _ _ (A.IHead _ _ xs)) = unions (map derivDeclTypes' xs) -- Just (moduleName, sName x)
      derivDeclTypes (A.DerivDecl a b (A.IHParen _ x)) = derivDeclTypes (A.DerivDecl a b x)
      derivDeclTypes (A.DerivDecl _ _ (A.IHInfix _ x _op y)) = union (derivDeclTypes' x) (derivDeclTypes' y)
      derivDeclTypes _ = empty
      -- derivDeclTypes' :: Type -> Set (Maybe S.ModuleName, S.Name)
      derivDeclTypes' (A.TyForall _ _ _ x) = derivDeclTypes' x -- qualified type
      derivDeclTypes' (A.TyFun _ x y) = union (derivDeclTypes' x) (derivDeclTypes' y) -- function type
      derivDeclTypes' (A.TyTuple _ _ xs) = unions (map derivDeclTypes' xs) -- tuple type, possibly boxed
      derivDeclTypes' (A.TyList _ x) =  derivDeclTypes' x -- list syntax, e.g. [a], as opposed to [] a
      derivDeclTypes' (A.TyApp _ x y) = union (derivDeclTypes' x) (derivDeclTypes' y) -- application of a type constructor
      derivDeclTypes' (A.TyVar _ _) = empty -- type variable
      derivDeclTypes' (A.TyCon _ (A.Qual _ m n)) = singleton (Just (sModuleName m), sName n) -- named type or type constructor
      -- Unqualified names refer to imports without "qualified" or "as" values.
      derivDeclTypes' (A.TyCon _ (A.UnQual _ n)) = singleton (Nothing, sName n)
      derivDeclTypes' (A.TyCon _ _) = empty
      derivDeclTypes' (A.TyParen _ x) = derivDeclTypes' x -- type surrounded by parentheses
      derivDeclTypes' (A.TyInfix _ x _op y) = union (derivDeclTypes' x) (derivDeclTypes' y) -- infix type constructor
      derivDeclTypes' (A.TyKind _ x _) = derivDeclTypes' x -- type with explicit kind signature

-- | Compare the two import declarations ignoring the things that are
-- actually being imported.  Equality here indicates that the two
-- imports could be merged.
importMergable :: A.ImportDecl SrcSpanInfo -> A.ImportDecl SrcSpanInfo -> Ordering
importMergable a b =
    case (compare `on` noSpecs) a' b' of
      EQ -> EQ
      specOrdering ->
          case (compare `on` S.importModule) a' b' of
            EQ -> specOrdering
            moduleNameOrdering -> moduleNameOrdering
    where
      a' = sImportDecl a
      b' = sImportDecl b
      -- Return a version of an ImportDecl with an empty spec list and no
      -- source locations.  This will distinguish "import Foo as F" from
      -- "import Foo", but will let us group imports that can be merged.
      -- Don't merge hiding imports with regular imports.
      noSpecs :: S.ImportDecl -> S.ImportDecl
      noSpecs x = x { S.importLoc = def,
                      S.importSpecs = case S.importSpecs x of
                                        Just (True, _) -> Just (True, []) -- hiding
                                        Just (False, _) -> Nothing
                                        Nothing -> Nothing }

-- | Be careful not to try to compare objects with embeded SrcSpanInfo.
unModuleName :: A.ModuleName SrcSpanInfo -> String
unModuleName (A.ModuleName _ x) = x

-- Compare function used to sort the symbols within an import.
compareSpecs :: A.ImportSpec SrcSpanInfo -> A.ImportSpec SrcSpanInfo -> Ordering
compareSpecs a b =
    case compare (map (map toLower . nameString) $ catMaybes $ toList $ symbols a) (map (map toLower . nameString) $ catMaybes $ toList $ symbols b) of
      EQ -> compare (sImportSpec a) (sImportSpec b)
      x -> x

equalSpecs :: A.ImportSpec SrcSpanInfo -> A.ImportSpec SrcSpanInfo -> Bool
equalSpecs a b = compareSpecs a b == EQ

-- dropSuffix :: Eq a => [a] -> [a] -> [a]
-- dropSuffix suf x = if isSuffixOf suf x then take (length x - length suf) x else x

-- dropPrefix :: Eq a => [a] -> [a] -> [a]
-- dropPrefix pre x = if isPrefixOf pre x then drop (length x) x else x

nameString :: S.Name -> String
nameString (S.Ident s) = s
nameString (S.Symbol s) = s

tests :: Test
tests = TestLabel "Clean" (TestList [test1, test2, test3, test4, test5, test6])

test1 :: Test
test1 =
    TestLabel "Imports.test1" $ TestCase
      (do _ <- system "rsync -aHxS --delete testdata/original/ testdata/copy"
          let name = S.ModuleName "Debian.Repo.Types.PackageIndex"
          let base = modulePathBase name
          _ <- withCurrentDirectory "testdata/copy" (runMonadClean (cleanImports base))
          (code, diff, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/original" </> base, "testdata/copy" </> base] ""
          assertEqual "cleanImports"
                         (ExitFailure 1,
                          ["@@ -22,13 +22,13 @@",
                           "     , prettyPkgVersion",
                           "     ) where",
                           " ",
                           "-import Data.Text (Text, map)",
                           "+import Data.Text (Text)",
                           " import Debian.Arch (Arch(..))",
                           " import qualified Debian.Control.Text as T (Paragraph)",
                           " import Debian.Relation (BinPkgName(..), SrcPkgName(..))",
                           " import qualified Debian.Relation as B (PkgName, Relations)",
                           " import Debian.Release (Section(..))",
                           "-import Debian.Repo.Orphans ({- instances -})",
                           "+import Debian.Repo.Orphans ()",
                           " import Debian.Version (DebianVersion, prettyDebianVersion)",
                           " import System.Posix.Types (FileOffset)",
                           " import Text.PrettyPrint.ANSI.Leijen ((<>), Doc, Pretty(pretty), text)"],
                          "")
                          (code, drop 2 (lines diff), err))

test2 :: Test
test2 =
    TestLabel "Imports.test2" $ TestCase
      (do _ <- system "rsync -aHxS --delete testdata/original/ testdata/copy"
          let name = S.ModuleName "Debian.Repo.PackageIndex"
              base = modulePathBase name
          _ <- withCurrentDirectory "testdata/copy" (runMonadClean (cleanImports base))
          (code, diff, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/original" </> base, "testdata/copy" </> base] ""
          assertEqual "cleanImports" (ExitSuccess, "", "") (code, diff, err))

-- | Can we handle a Main module in a file named something other than Main.hs?
test3 :: Test
test3 =
    TestLabel "Imports.test3" $ TestCase
      (runMonadClean (modifyParams (\ p -> p {sourceDirs = ["testdata"]}) >> cleanImports "testdata/NotMain.hs") >>
       assertEqual "module name" () ())

-- | Preserve imports with a "hiding" clause
test4 :: Test
test4 =
    TestLabel "Imports.test4" $ TestCase
      (system "cp testdata/HidingOrig.hs testdata/Hiding.hs" >>
       runMonadClean (modifyParams (\ p -> p {sourceDirs = ["testdata"]}) >> cleanImports "testdata/Hiding.hs") >>
       -- Need to check the text of Hiding.hs, but at least this verifies that there was no crash
       assertEqual "module name" () ())

-- | Preserve imports used by a standalone deriving declaration
test5 :: Test
test5 =
    TestLabel "Imports.test5" $ TestCase
      (do _ <- system "cp testdata/DerivingOrig.hs testdata/Deriving.hs"
          _ <- runMonadClean (modifyParams (\ p -> p {extensions = extensions p ++ [StandaloneDeriving, TypeSynonymInstances, FlexibleInstances],
                                                  sourceDirs = ["testdata"]}) >>
                          cleanImports "testdata/Deriving.hs")
          (code, diff, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/DerivingOrig.hs", "testdata/Deriving.hs"] ""
          assertEqual "standalone deriving"
                      (ExitFailure 1,
                       (unlines
                        ["@@ -1,7 +1,6 @@",
                         " module Deriving where",
                         " ",
                         "-import Data.Text (Text)",
                         "-import Debian.Control (Paragraph(..), Paragraph'(..), Field'(..))",
                         "+import Debian.Control (Field'(..), Paragraph(..))",
                         " ",
                         " deriving instance Show (Field' String)",
                         " deriving instance Show Paragraph"]),
                       "")
                      (code, unlines (drop 2 (lines diff)), err))

-- | Comment at EOF
test6 :: Test
test6 =
    TestLabel "Imports.test6" $ TestCase
      (do -- _ <- system "rsync -aHxS --delete testdata/logic/ testdata/copy"
          _ <- system "cp testdata/EndCommentOrig.hs testdata/EndComment.hs"
          let path = "EndComment.hs" -- "Data/Logic/Harrison/Tableaux.hs"
          _ <- withCurrentDirectory "testdata" (runMonadClean (modifyTestMode (const True) >> cleanImports "EndComment.hs"))
          (code, diff, err) <- readProcessWithExitCode "diff" ["-ru", "testdata/EndCommentClean.hs", "testdata" </> "EndComment.hs"] ""
          assertEqual "comment at end" "" diff)
