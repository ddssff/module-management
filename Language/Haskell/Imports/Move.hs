{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Move
    ( moveImports
    , test2
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try)
import Data.List (findIndex, tails)
import Data.Maybe (fromJust)
import Language.Haskell.Exts.Syntax (ImportDecl(ImportDecl, importModule, importSpecs), ImportSpec, Module(Module), ModuleName(..))
import Language.Haskell.Exts (parseFile, ParseResult(ParseOk))
import Language.Haskell.Imports.Common (importsSpan, renameSpec, replaceChangedImports)

type FQID = String -- ^ Fully qualified identifier - e.g. Language.Haskell.Imports.Clean.cleanImports

parseFQID :: FQID -> (ModuleName, String)
parseFQID s =
    let (m, n) = splitAt (fromJust (findIndex (not . elem '.') (tails s)) - 1) s in
    (ModuleName m, (tail n))

-- | This function needs to be able to compile the source file, so it
-- must be run *before* the declaration actually gets moved to its
-- new module.
moveImports :: Bool -> [(FQID, FQID)] -> FilePath -> IO ()
moveImports dryRun moves sourcePath =
    do source <- try ((,) <$> parseFile sourcePath <*> readFile sourcePath)
       case source of
         Left (e :: SomeException) -> error (sourcePath ++ ": " ++ show e)
         Right (ParseOk m@(Module _ _ _ _ _ oldImports _), sourceText) ->
             replaceChangedImports dryRun oldImports (doMoves moves oldImports) sourceText sourcePath (importsSpan m)
         Right _ -> error (sourcePath ++ ": could not parse")

doMoves :: [(FQID, FQID)] -> [ImportDecl] -> [ImportDecl]
doMoves moves imports =
    foldr moveDecls [] moves
    where
      moveDecls :: (FQID, FQID) -> [ImportDecl] -> [ImportDecl]
      moveDecls (src, dst) decls =
          foldr moveDecl decls imports
          where
            (srcM, srcN) = parseFQID src
            (dstM, dstN) = parseFQID dst
            moveDecl :: ImportDecl -> [ImportDecl] -> [ImportDecl]
            moveDecl decl@(ImportDecl {importSpecs = Nothing}) result = decl : result
            moveDecl decl@(ImportDecl {importModule = m, importSpecs = Just (flag, specs)}) result =
                [decl {importSpecs = Just (flag, specs')}] ++ decls' ++ result
                where
                  (specs', decls') = foldr moveSpec ([], []) specs
                  moveSpec :: ImportSpec -> ([ImportSpec], [ImportDecl]) -> ([ImportSpec], [ImportDecl])
                  moveSpec spec (specs'', decls'') =
                      if m == srcM && spec == srcSpec
                      then (specs'', (decl {importModule = dstM, importSpecs = Just (False, [dstSpec])} : decls''))
                      else (spec : specs'', decls'')
                      where
                        srcSpec = renameSpec srcN spec
                        dstSpec = renameSpec dstN spec

test2 :: IO ()
test2 = moveImports True [("Language.Haskell.Imports.Clean.moveImports", "Language.Haskell.Imports.Move.moveImports")] "Language/Haskell/Imports/Clean.hs"
