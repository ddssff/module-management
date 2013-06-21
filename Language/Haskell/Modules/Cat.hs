{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Cat
    ( catModules
    , tests
    , test1
    , test2
    , test3
    ) where

import Control.Monad as List (mapM)
import Control.Monad.Trans (liftIO)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.List as List (intercalate, map)
import Data.Map as Map (fromList, lookup, Map, member, toAscList, insert)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (fromList, insert, Set, union, difference, toList, null)
import Data.Set.Extra as Set (mapM)
import Language.Haskell.Exts.Annotated.Simplify (sDecl, sExportSpec, sModuleName, sImportDecl)
import qualified Language.Haskell.Exts.Annotated.Syntax as A (ExportSpecList(ExportSpecList), Module(Module), ModuleHead(ModuleHead), ImportDecl(..))
import Language.Haskell.Exts.Parser (fromParseResult)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import qualified Language.Haskell.Exts.Syntax as S (ExportSpec(EModuleContents), ImportDecl(..), ModuleName(..))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Fold (foldHeader, foldExports, foldImports, foldDecls, echo, echo2, ignore, ignore2)
import Language.Haskell.Modules.Imports (cleanImports)
import Language.Haskell.Modules.Params (modifyParams, modulePath, MonadClean, Params(sourceDirs, moduVerse, testMode), parseFile, runCleanT, getParams, ModuleResult(..), doResult)
import Language.Haskell.Modules.Util.Test (diff, repoModules)
import System.Cmd (system)
import System.Exit (ExitCode(ExitSuccess))
import Test.HUnit (assertEqual, Test(TestCase, TestList))

-- | Merge the declarations from several modules into a single new
-- one.  Note that a circular imports can be created by this
-- operation, in which case you will have to add more modules to the
-- merge.
catModules :: MonadClean m => Set S.ModuleName -> [S.ModuleName] -> S.ModuleName -> m (Set ModuleResult)
catModules univ inputs output =
    do testModuVerse univ
       let univ' = union univ (Set.fromList (output : inputs))
       inputInfo <- loadModules inputs
       result <- Set.mapM (doModule inputInfo inputs output) univ' >>= Set.mapM doResult
      -- The inputs disappear and the output appears.  If the output is one
      -- of the inputs, it does not disappear.
       modifyParams (\ p -> p {moduVerse = fmap (\ s -> Set.insert output (Set.difference s (Set.fromList inputs))) (moduVerse p)})
       Set.mapM clean result
    where
      clean x =
          do flag <- getParams >>= return . not . testMode
             case x of
               (Modified name _) | flag -> modulePath name >>= cleanImports
               _ -> return x

-- Process one of the modules in the moduVerse and return the result.
-- The output module may not (yet) be an element of the moduVerse, in
-- that case choose the first input modules to convert into the output
-- module.
doModule :: MonadClean m => Map S.ModuleName (A.Module SrcSpanInfo, String) -> [S.ModuleName] -> S.ModuleName -> S.ModuleName -> m ModuleResult
doModule inputInfo inputs@(first : _) output name =
    do -- The new module will be based on the existing module, unless
       -- name equals output and output does not exist
       let oldName = if name == output && not (Map.member name inputInfo) then first else name
       (m, text) <- maybe (loadModule oldName) return (Map.lookup name inputInfo)
       return $ case () of
         _ | name == output -> Modified name (doOutput inputInfo inputs output (m, text))
           | elem name inputs -> Removed name
         _ -> let text' = doOther inputs output (m, text) in
              if text /= text' then Modified name text' else Unchanged name
doModule _ [] _ _ = error "doModule: no inputs"

-- | Create the output module, the destination of the cat.
doOutput :: Map S.ModuleName (A.Module SrcSpanInfo, String) -> [S.ModuleName] -> S.ModuleName -> (A.Module SrcSpanInfo, String) -> String
doOutput inputInfo inNames outName (m, text) =
    header ++ exports ++ imports ++ decls
    where
      header = foldHeader echo2 echo (\ _ pref _ suff r -> r <> pref <> prettyPrint outName <> suff) echo m text "" <>
               foldExports (\ s r -> r <> s <> maybe "" (intercalate ", " . List.map (prettyPrint)) (mergeExports inputInfo outName) <> "\n") ignore ignore2 m text ""
      exports = fromMaybe "" (foldExports ignore2 (\ _e pref _ _ r -> maybe (Just pref) Just r) ignore2 m text Nothing)
      imports = foldExports ignore2 ignore (\ s r -> r <> s {-where-}) m text "" <>
                -- Insert the new imports just after the first "pre" string of the imports
                (fromMaybe "" (foldImports (\ _ pref _ _ r -> maybe (Just (pref <> unlines (List.map (moduleImports inputInfo) inNames))) Just r) m text Nothing)) <>
                (foldImports (\ _i pref s suff r -> r <> pref <> s <> suff) m text "")
      decls = fromMaybe "" (foldDecls (\ _d _ _ _ r -> Just (fromMaybe (unlines (List.map (moduleDecls inputInfo outName) inNames)) r)) (\ s r -> Just (maybe s (<> s) r)) m text Nothing)

-- | Update a module that does not participate in the cat - this
-- involves changing imports and exports of catted modules.
-- (Shouldn't this also fix qualified symbols?)
doOther :: [S.ModuleName] -> S.ModuleName -> (A.Module SrcSpanInfo, String) -> String
doOther inputs output (m, text) =
    foldHeader echo2 echo echo echo m text "" <>
    foldExports echo2 (\ x pref s suff r -> r <> pref <> fromMaybe s (fixModuleExport inputs output (sExportSpec x)) <> suff) echo2 m text "" <>
    foldImports (\ x pref s suff r -> r <> pref <> fromMaybe s (fixModuleImport inputs output (sImportDecl x)) <> suff) m text "" <>
    foldDecls echo echo2 m text ""

fixModuleExport :: [S.ModuleName] -> S.ModuleName -> S.ExportSpec -> Maybe String
fixModuleExport inputs output x =
          case x of
            S.EModuleContents y
                | elem y inputs -> Just (prettyPrint (S.EModuleContents output))
            _ -> Nothing

fixModuleImport :: [S.ModuleName] -> S.ModuleName -> S.ImportDecl -> Maybe String
fixModuleImport inputs output x =
          case x of
            S.ImportDecl {S.importModule = y}
                | elem y inputs -> Just (prettyPrint (x {S.importModule = output}))
            _ -> Nothing

mergeExports :: Map S.ModuleName (A.Module SrcSpanInfo, String) -> S.ModuleName -> Maybe [S.ExportSpec]
mergeExports old new =
    Just (concatMap mergeExports' (Map.toAscList old))
    where
      mergeExports' (_, (A.Module _ Nothing _ _ _, _)) = error "catModules: no explicit export list"
      mergeExports' (_, (A.Module _ (Just (A.ModuleHead _ _ _ Nothing)) _ _ _, _)) = error "catModules: no explicit export list"
      mergeExports' (_, (A.Module _ (Just (A.ModuleHead _ _ _ (Just (A.ExportSpecList _ e)))) _ _ _, _)) = updateModuleContentsExports old new (List.map sExportSpec e)
      mergeExports' (_, _) = error "mergeExports'"

updateModuleContentsExports :: Map S.ModuleName (A.Module SrcSpanInfo, String) -> S.ModuleName -> [S.ExportSpec] -> [S.ExportSpec]
updateModuleContentsExports old new es =
    foldl f [] es
    where
      f :: [S.ExportSpec] -> S.ExportSpec ->  [S.ExportSpec]
      f ys (S.EModuleContents m) =
          let e' = S.EModuleContents (if Map.member m old then new else m) in
          ys ++ if elem e' ys then [] else [e']
      f ys e = ys ++ [e]

moduleImports :: Map S.ModuleName (A.Module SrcSpanInfo, String) -> S.ModuleName -> String
moduleImports old name =
    let (Just (m, text)) = Map.lookup name old in
    foldImports (\ x pref s suff r ->
                    r
                    -- If this is the first import, omit the prefix, it includes the ") where" text.
                    <> (if r == "" then "" else pref)
                    <> if Map.member (sModuleName (A.importModule x)) old then "" else (s <> suff))
                m text "" <> "\n"

-- | Grab the declarations out of the old modules, fix any
-- qualified symbol references, prettyprint and return.
--
-- Bug: If we cat two modules A and B, and A imported a symbol from B
-- and referenced that symbol with a qualifier from an "as" import, the
-- as qualifier needs to be changed to a full qualifier.
--
-- In terms of what is going on right here, if m imports any of the
-- modules in oldmap with an "as" qualifier, identifiers using the
-- module name in the "as" qualifier must use new instead.
moduleDecls :: Map S.ModuleName (A.Module SrcSpanInfo, String) -> S.ModuleName -> S.ModuleName -> String
moduleDecls oldmap new name =
    let (Just (m@(A.Module _ _ _ imports _), text)) = Map.lookup name oldmap in
    let oldmap' = foldr f oldmap imports in
    foldDecls (\ d pref s suff r ->
                    let d' = sDecl d
                        d'' = fixReferences oldmap' new d' in
                    r <> pref <>
                    (if d'' /= d' then "-- Declaration reformatted because module qualifiers changed\n" <> prettyPrint d'' <> "\n\n" else (s <> suff)))
              echo2
              m text "" <> "\n"
    where
      f (A.ImportDecl _ m _ _ _ (Just a) _specs) mp =
          case Map.lookup (sModuleName m) oldmap of
            Just x -> Map.insert (sModuleName a) x mp
            _ -> mp
      f _ mp = mp

-- | Change any ModuleName in 'old' to 'new'.  Note that this will
-- probably mess up the location information, so the result (if
-- different from the original) should be prettyprinted, not
-- exactPrinted.
fixReferences :: (Data a, Typeable a) => Map S.ModuleName (A.Module SrcSpanInfo, String) -> S.ModuleName -> a -> a
fixReferences oldmap new x =
    everywhere (mkT moveModuleName) x
    where
      moveModuleName :: S.ModuleName -> S.ModuleName
      moveModuleName name@(S.ModuleName _) = if Map.member name oldmap then new else name

-- Just for testing
testModuVerse :: MonadClean m => Set S.ModuleName -> m ()
testModuVerse s =
    getParams >>= maybe (error "ModuVerse not set") message . moduVerse
    where
      message p =
          case (difference s p, difference p s) of
            (extra, missing) | not (Set.null extra && Set.null missing) -> error $ "moduVerse mismatch, missing: " ++ show (toList extra) ++ ", extra: " ++ show (toList missing)
            _ -> return ()

tests :: Test
tests = TestList [test1, test2, test3]

test1 :: Test
test1 =
    TestCase $
      do _ <- system "rsync -aHxS --delete testdata/original/ testdata/copy"
         _result <- runCleanT $
           do modifyParams (\ p -> p {sourceDirs = ["testdata/copy"], moduVerse = Just repoModules})
              catModules
                     repoModules
                     [S.ModuleName "Debian.Repo.AptCache", S.ModuleName "Debian.Repo.AptImage"]
                     (S.ModuleName "Debian.Repo.Cache")
              -- mapM_ (removeFileIfPresent . ("testdata/copy" </>)) junk
         (code, out, err) <- diff "testdata/catresult1" "testdata/copy"
         assertEqual "catModules1" (ExitSuccess, "", "") (code, out, err)

test2 :: Test
test2 =
    TestCase $
      do _ <- system "rsync -aHxS --delete testdata/original/ testdata/copy"
         _result <- runCleanT $
           do modifyParams (\ p -> p {sourceDirs = ["testdata/copy"], moduVerse = Just repoModules})
              catModules
                     repoModules
                     [S.ModuleName "Debian.Repo.Types.Slice", S.ModuleName "Debian.Repo.Types.Repo", S.ModuleName "Debian.Repo.Types.EnvPath"]
                     (S.ModuleName "Debian.Repo.Types.Common")
              -- mapM_ (removeFileIfPresent . ("testdata/copy" </>)) junk
         (code, out, err) <- diff "testdata/catresult2" "testdata/copy"
         assertEqual "catModules2" (ExitSuccess, "", "") (code, out, err)

test3 :: Test
test3 =
    TestCase $
      do _ <- system "rsync -aHxS --delete testdata/original/ testdata/copy"
         _result <- withCurrentDirectory "testdata/copy" $
                   runCleanT $
           do modifyParams (\ p -> p {moduVerse = Just repoModules})
              catModules
                     repoModules
                     [S.ModuleName "Debian.Repo.Types.Slice",
                      S.ModuleName "Debian.Repo.Types.Repo",
                      S.ModuleName "Debian.Repo.Types.EnvPath"]
                     (S.ModuleName "Debian.Repo.Types.Slice")
              -- mapM_ (removeFileIfPresent . ("testdata/copy" </>)) junk
         (code, out, err) <- diff "testdata/catresult3" "testdata/copy"
         assertEqual "catModules3" (ExitSuccess, "", "") (code, out, err)

-- junk :: String -> Bool
-- junk s = isSuffixOf ".imports" s || isSuffixOf "~" s

loadModules :: MonadClean m => [S.ModuleName] -> m (Map S.ModuleName (A.Module SrcSpanInfo, String))
loadModules names = List.mapM loadModule names >>= return . Map.fromList . zip names

loadModule :: MonadClean m => S.ModuleName -> m (A.Module SrcSpanInfo, String)
loadModule name =
    do path <- modulePath name
       text <- liftIO $ readFile path
       m <- parseFile path >>= return . fromParseResult
       return (m, text)
