{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Move
    ( moveImports
    , test2
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try)
import Control.Monad.Trans (liftIO)
import Data.List (findIndex, tails)
import Data.Maybe (fromJust)
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(ParseOk))
import Language.Haskell.Exts.Syntax (ImportDecl(ImportDecl, importModule, importSpecs), ImportSpec, Module(Module), ModuleName(..))
import Language.Haskell.Imports.Common (importsSpan, renameSpec, replaceFile, replaceImports, tildeBackup)
import Language.Haskell.Imports.Params (MonadParams, putDryRun, runParamsT)

type FQID = String -- ^ Fully qualified identifier - e.g. Language.Haskell.Imports.Clean.cleanImports

parseFQID :: FQID -> (ModuleName, String)
parseFQID s =
    let (m, n) = splitAt (fromJust (findIndex (not . elem '.') (tails s)) - 1) s in
    (ModuleName m, (tail n))

-- | Modify the imports of a source file to reflect the changes
-- described in the list of FQID pairs.  This function needs to be
-- able to compile the source file, so it must be run *before* the
-- declaration actually gets moved to its new module.
moveImports :: MonadParams m => [(FQID, FQID)] -> FilePath -> m ()
moveImports moves sourcePath =
    do source <- liftIO $ try ((,) <$> parseFileWithComments defaultParseMode sourcePath <*> readFile sourcePath)
       case source of
         Left (e :: SomeException) -> error (sourcePath ++ ": " ++ show e)
         Right (ParseOk (m@(Module _ _ _ _ _ oldImports _), comments), sourceText) ->
             maybe (liftIO $ putStrLn (sourcePath ++ ": no changes"))
                   (\ text ->
                        liftIO (putStrLn (sourcePath ++ ": replacing imports")) >>
                        replaceFile tildeBackup sourcePath text)
                   (replaceImports oldImports (doMoves moves oldImports) sourceText (importsSpan m comments))
         Right _ -> error (sourcePath ++ ": could not parse")

doMoves :: [(FQID, FQID)] -> [ImportDecl] -> [ImportDecl]
doMoves moves decls =
    foldr moveDecls decls moves -- For each of the FQID pairs do a pass through the decls
    where
      moveDecls :: (FQID, FQID) -> [ImportDecl] -> [ImportDecl]
      moveDecls (src, dst) decls' =
          foldr moveDecl [] decls'
          where
            (srcM, srcN) = parseFQID src
            (dstM, dstN) = parseFQID dst
            moveDecl :: ImportDecl -> [ImportDecl] -> [ImportDecl]
            moveDecl decl@(ImportDecl {importSpecs = Nothing}) result = decl : result
            moveDecl decl@(ImportDecl {importModule = m, importSpecs = Just (flag, specs)}) result =
                [decl {importSpecs = Just (flag, specs')}] ++ decls'' ++ result
                where
                  (specs', decls'') = foldr moveSpec ([], []) specs
                  moveSpec :: ImportSpec -> ([ImportSpec], [ImportDecl]) -> ([ImportSpec], [ImportDecl])
                  moveSpec spec (specs'', decls''') =
                      -- If we looking at the old symbol, remove it
                      -- and add an import of the new symbol.  Later
                      -- the cleanup code might remove it.
                      if m == srcM && spec == srcSpec
                      then (specs'', (decl {importModule = dstM, importSpecs = Just (False, [dstSpec])} : decls'''))
                      else (spec : specs'', decls''')
                      where
                        srcSpec = renameSpec srcN spec
                        dstSpec = renameSpec dstN spec

test2 :: MonadParams m => m ()
test2 = runParamsT
          (putDryRun True >>
           moveImports [("Language.Haskell.Imports.Clean.moveImports", "Language.Haskell.Imports.Move.moveImports")] "Language/Haskell/Imports/Clean.hs")
