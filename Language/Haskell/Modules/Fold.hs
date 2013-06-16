{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Fold
    ( foldModule
    , foldHeader
    , foldExports
    , foldImports
    , foldDecls
    , tests
    ) where

import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.Default (Default(def))
import Data.Set.Extra as Set (fromList)
import Data.Tree (Tree(..) {-, drawTree-})
import Language.Haskell.Exts.Annotated (ParseResult(..))
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl, ExportSpec, ExportSpecList(ExportSpecList), ImportDecl, ExportSpec(..), Module(..), ModuleHead(..), ModuleName, ModulePragma, WarningText)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Params (runCleanT, parseFileWithComments)
import Language.Haskell.Modules.Util.SrcLoc (HasSpanInfo(..), srcLoc, endLoc, textEndLoc, srcPairText, makeTree)
import Test.HUnit (assertEqual, Test(TestList, TestCase, TestLabel))

type Module = A.Module SrcSpanInfo
--type ModuleHead = A.ModuleHead SrcSpanInfo
type ModulePragma = A.ModulePragma SrcSpanInfo
type ModuleName = A.ModuleName SrcSpanInfo
type WarningText = A.WarningText SrcSpanInfo
type ExportSpec = A.ExportSpec SrcSpanInfo
type ImportDecl = A.ImportDecl SrcSpanInfo
type Decl = A.Decl SrcSpanInfo

class Spans a where
    spans :: a -> [SrcSpanInfo]

instance Spans (A.Module SrcSpanInfo) where
    spans (A.Module _sp mh ps is ds) =
        concatMap spans ps ++ maybe [] spans mh ++ concatMap spans is ++ concatMap spans ds
    spans _ = error "spans XML module"

instance Spans (A.ModuleHead SrcSpanInfo) where
    spans (A.ModuleHead _ n mw me) = spans n ++ maybe [] spans mw ++ maybe [] spans me

instance Spans (A.ExportSpecList SrcSpanInfo) where
    spans (A.ExportSpecList _ es) = concatMap spans es

instance Spans (A.ExportSpec SrcSpanInfo) where spans x = [spanInfo x]
instance Spans (A.ModulePragma SrcSpanInfo) where spans x = [spanInfo x]
instance Spans (A.ImportDecl SrcSpanInfo) where spans x = [spanInfo x]
instance Spans (A.Decl SrcSpanInfo) where spans x = [spanInfo x]
instance Spans (A.ModuleName SrcSpanInfo) where spans x = [spanInfo x]
instance Spans (A.WarningText SrcSpanInfo) where spans x = [spanInfo x]

-- | Given the result of parseModuleWithComments and the original
-- module text, this does a fold over the parsed module contents,
-- calling the seven argument functions in order.  Each function is
-- passed the AST value, the text of the space and comments leading up
-- to the element, and the text for the element.  Note that not
-- everything passed to the "pre" argument of the functions will be
-- comments and space - for example, the "module" keyword will be
-- passed in the pre argument to the ModuleName function.
foldModule :: forall r. (Show r) =>
              (String -> r -> r)
           -> (ModulePragma -> String -> String -> String -> r -> r)
           -> (ModuleName -> String -> String -> String -> r -> r)
           -> (WarningText -> String -> String -> String -> r -> r)
           -> (String -> r -> r)
           -> (ExportSpec -> String -> String -> String -> r -> r)
           -> (String -> r -> r)
           -> (ImportDecl -> String -> String -> String -> r -> r)
           -> (Decl -> String -> String -> String -> r -> r)
           -> (String -> r -> r)
           -> Module -> [Comment] -> String -> r -> r
foldModule _ _ _ _ _ _ _ _ _ _ (A.XmlPage _ _ _ _ _ _ _) _ _ _ = error "XmlPage: unsupported"
foldModule _ _ _ _ _ _ _ _ _ _ (A.XmlHybrid _ _ _ _ _ _ _ _ _) _ _ _ = error "XmlHybrid: unsupported"
foldModule topf pragmaf namef warnf pref exportf postf importf declf sepf m@(A.Module _ mh ps is ds) _comments text r0 =
    let (r1, l1, sps1) = doSep text topf (r0, def, spans m)
        (r2, l2, sps2) = doList text pragmaf ps (r1, l1, sps1)
        (r8, l8, sps8) =
            case mh of
              Nothing -> (r2, l2, sps2)
              Just (A.ModuleHead sp n mw me) ->
                  let (r3, l3, sps3) = doItem text namef n (r2, l2, sps2)
                      (r4, l4, sps4) = case mw of
                                         Nothing -> (r3, l3, sps3)
                                         Just w -> doItem text warnf w (r3, l3, sps3)
                      (r5, l5, sps5) = doSep text pref (r4, l4, sps4)
                      (r6, l6, sps6) = case me of
                                         Nothing -> (r5, l5, sps5)
                                         Just (A.ExportSpecList _ es) -> doList text exportf es (r5, l5, sps5)
                      -- Do separator text through end of header
                      (r7, l7, _sps7) = if l6 < endLoc sp
                                       then (postf (srcPairText l6 (endLoc sp) text) r6, endLoc sp, sps6)
                                       else (r6, l6, sps6) in
                  (r7, l7, sps6)
        (r9, l9, sps9) = doList text importf is (r8, l8, sps8)
        (r10, l10, _sps10) = doList text declf ds (r9, l9, sps9)
        r11 = if l10 < textEndLoc text then sepf (srcPairText l10 (textEndLoc text) text) r10 else r10
    in
    r11

foldHeader :: forall r. (Show r) =>
              (String -> r -> r)
           -> (ModulePragma -> String -> String -> String -> r -> r)
           -> (ModuleName -> String -> String -> String -> r -> r)
           -> (WarningText -> String -> String -> String -> r -> r)
           -> Module -> [Comment] -> String -> r -> r
foldHeader topf pragmaf namef warnf m comments text r0 =
    foldModule topf pragmaf namef warnf
               (\ _ r -> r) (\ _ _ _ _ r -> r) (\ _ r -> r)
               (\ _ _ _ _ r -> r)
               (\ _ _ _ _ r -> r) (\ _ r -> r)
               m comments text r0

foldExports :: forall r. (Show r) =>
               (String -> r -> r)
            -> (ExportSpec -> String -> String -> String -> r -> r)
            -> (String -> r -> r)
            -> Module -> [Comment] -> String -> r -> r
foldExports pref exportf postf m comments text r0 =
    foldModule (\ _ r -> r) (\ _ _ _ _ r -> r) (\ _ _ _ _ r -> r) (\ _ _ _ _ r -> r)
               pref exportf postf
               (\ _ _ _ _ r -> r)
               (\ _ _ _ _ r -> r) (\ _ r -> r)
               m comments text r0

foldImports :: forall r. (Show r) =>
               (ImportDecl -> String -> String -> String -> r -> r)
            -> Module -> [Comment] -> String -> r -> r
foldImports importf m comments text r0 =
    foldModule (\ _ r -> r) (\ _ _ _ _ r -> r) (\ _ _ _ _ r -> r) (\ _ _ _ _ r -> r)
               (\ _ r -> r) (\ _ _ _ _ r -> r) (\ _ r -> r)
               importf
               (\ _ _ _ _ r -> r) (\ _ r -> r)
               m comments text r0

foldDecls :: forall r. (Show r) =>
             (Decl -> String -> String -> String -> r -> r)
          -> (String -> r -> r)
          -> Module -> [Comment] -> String -> r -> r
foldDecls declf sepf m comments text r0 =
    foldModule (\ _ r -> r) (\ _ _ _ _ r -> r) (\ _ _ _ _ r -> r) (\ _ _ _ _ r -> r)
               (\ _ r -> r) (\ _ _ _ _ r -> r) (\ _ r -> r)
               (\ _ _ _ _ r -> r)
               declf sepf
               m comments text r0

doSep :: String
      -> (String -> r -> r)
      -> (r, SrcLoc, [SrcSpanInfo])
      -> (r, SrcLoc, [SrcSpanInfo])
doSep text sepf (r, l, sps@(sp : _)) =
    if l <= l' then (r', l', sps) else (r, l, sps)
    where
      r' = sepf (srcPairText l l' text) r
      l' = srcLoc sp
doSep _ _ (_, _, []) = error "doSep: no spans!"

doList :: (HasSpanInfo a, Show a) =>
          String
       -> (a -> String -> String -> String -> r -> r)
       -> [a]
       -> (r, SrcLoc, [SrcSpanInfo])
       -> (r, SrcLoc, [SrcSpanInfo])
doList _ _ [] (r, l, sps) = (r, l, sps)
doList text f (x : xs) (r, l, sps) =
    let (r', l', sps') = doItem text f x (r, l, sps) in
    doList text f xs (r', l', sps')

-- Very slow due to uses of srcPairText.
doItem :: (HasSpanInfo a, Show a) =>
          String
       -> (a -> String -> String -> String -> r -> r)
       -> a
       -> (r, SrcLoc, [SrcSpanInfo]) -- The current result value and location in the text
       -> (r, SrcLoc, [SrcSpanInfo])
doItem _ _ _ (r, l, []) = (r, l, []) -- error $ "doItem - No Spans: " ++ int x ++ ", l=" ++ show l
doItem text f x (r, l, (sp : sps')) =
    (r', l'', sps')
    where
      r' = f x pre s w r
      pre = srcPairText l m text
      s = srcPairText m l' text
      post = srcPairText l' (textEndLoc text) text
      (w, l'', _post') = adjust post l'
      m = srcLoc sp
      l' = endLoc sp

 -- adjust "\n  \n  hello\n" (SrcLoc "<unknown>.hs" 5 5) -> ("\n  \n", (SrcLoc "<unknown>.hs" 7 1), "  hello\n")
adjust :: String -> SrcLoc -> (String, SrcLoc, String)
adjust a l =
    (w'', l', a''')
    where
      (w, a') = break (not . isSpace) a
      (a'', w') = break (== '\n') (reverse w)
      w'' = reverse w'
      a''' = reverse a'' ++ a'
      l' = adjust' w'' l

adjust' :: String -> SrcLoc -> SrcLoc
adjust' "" l = l
adjust' ('\n' : s) (SrcLoc f y _) = adjust' s (SrcLoc f (y + 1) 1)
adjust' (_ : s) (SrcLoc f y x) = adjust' s (SrcLoc f y (x + 1))

tests :: Test
tests = TestLabel "Clean" (TestList [test1, test1b, test4, test6])

test1 :: Test
test1 =
    TestLabel "test1" $ TestCase $ withCurrentDirectory "testdata/original" $
    do let path = "Debian/Repo/Orphans.hs"
       text <- liftIO $ readFile path
       ParseOk (m, comments) <- runCleanT $ parseFileWithComments path
       let (output, original) = test m comments text
       assertEqual "echo" original output
    where
      test :: Module -> [Comment] -> String -> (String, String)
      test m comments text =
          (foldModule tailf pragmaf namef warningf tailf exportf tailf importf declf tailf m comments text "", text)
          where
            pragmaf :: ModulePragma -> String -> String -> String -> String -> String
            pragmaf _x pref s suff r = r ++ pref ++ s ++ suff
            -- pragmaf _x pre s r = r ++ "pragma: " ++ show s ++ "\n"
            namef :: ModuleName -> String -> String -> String -> String -> String
            namef _x pref s suff r = r ++ pref ++ s ++ suff
            -- namef _x pre s r = r ++ "name: " ++ show s ++ "\n"
            warningf :: WarningText -> String -> String -> String -> String -> String
            warningf _x pref s suff r = r ++ pref ++ s ++ suff
            -- warningf _x pre s r = r ++ "warning: " ++ show s ++ "\n"
            exportf :: ExportSpec -> String -> String -> String -> String -> String
            exportf _x pref s suff r = r ++ pref ++ s ++ suff
            -- exportf _x pre s r = r ++ "export: " ++ show s ++ "\n"
            importf :: ImportDecl -> String -> String -> String -> String -> String
            importf _x pref s suff r = r ++ pref ++ s ++ suff
            -- importf _x pre s r = r ++ "import: " ++ show s ++ "\n"
            declf :: Decl -> String -> String -> String -> String -> String
            declf _x pref s suff r = r ++ pref ++ s ++ suff
            -- declf _x pre s r = r ++ "decl: " ++ show s ++ "\n"
            tailf :: String -> String -> String
            tailf s r = r ++ s
            -- spacef "" r = r
            -- spacef s r = r ++ "space: " ++ show s ++ "\n"

test1b :: Test
test1b =
    TestLabel "test1b" $ TestCase $ withCurrentDirectory "testdata/original" $
    do let path = "Debian/Repo/Sync.hs"
       text <- liftIO $ readFile path
       ParseOk m <- runCleanT $ parseFileWithComments path
       let output = test m text
       assertEqual "echo"
                   [("-- Comment above module head\nmodule ","","",""),
                    ("","Debian.Repo.Sync","","[2.8:2.24]"),
                    (" ","{-# WARNING \"this is a warning\" #-}","\n","[2.25:2.60]"),
                    ("    ( ","","",""),
                    ("","rsync","\n","[3.7:3.12]"),
                    ("    , ","foo","\n","[4.7:4.10]"),
                    ("    -- Comment after last export\n    ) where","","",""),
                    ("\n\n-- Comment before first import\n\n","import Control.Monad.Trans (MonadIO)","\n","[10.1:10.37]"),
                    ("","import qualified Data.ByteString as B (empty)","\n","[11.1:11.46]"),
                    ("","import System.Exit (ExitCode)","\n","[12.1:12.30]"),
                    ("","import System.FilePath (dropTrailingPathSeparator)","\n","[13.1:13.51]"),
                    ("-- Comment between two imporrts\n","import System.Process (proc)","\n","[15.1:15.29]"),
                    ("","import System.Process.Progress (keepResult, runProcessF)","\n\n","[16.1:16.57]"),
                    ("-- Comment before first decl\n","rsync :: (Functor m, MonadIO m) => [String] -> FilePath -> FilePath -> m ExitCode","\n","[19.1:19.82]"),
                    ("","rsync extra source dest =\n    do result <- runProcessF (proc \"rsync\" ([\"-aHxSpDt\", \"--delete\"] ++ extra ++\n                                            [dropTrailingPathSeparator source ++ \"/\",\n                                             dropTrailingPathSeparator dest])) B.empty >>= return . keepResult\n       case result of\n         [x] -> return x\n         _ -> error \"Missing or multiple exit codes\"\n\n-- Comment between two decls\n","","[20.1:29.0]"),
                    ("","foo :: Int","\n","[29.1:29.11]"),
                    ("","foo = 1","\n\n","[30.1:30.8]"),
                    ("{-\nhandleExit 1 = \"Syntax or usage error\"\nhandleExit 2 = \"Protocol incompatibility\"\nhandleExit 3 = \"Errors selecting input/output files, dirs\"\nhandleExit 4 = \"Requested action not supported: an attempt was made to manipulate 64-bit files on a platform that cannot support them; or an option was specified that is supported by the client and not by the server.\"\nhandleExit 5 = \"Error starting client-server protocol\"\nhandleExit 6 = \"Daemon unable to append to log-file\"\nhandleExit 10 = \"Error in socket I/O\"\nhandleExit 11 = \"Error in file I/O\"\nhandleExit 12 = \"Error in rsync protocol data stream\"\nhandleExit 13 = \"Errors with program diagnostics\"\nhandleExit 14 = \"Error in IPC code\"\nhandleExit 20 = \"Received SIGUSR1 or SIGINT\"\nhandleExit 21 = \"Some error returned by waitpid()\"\nhandleExit 22 = \"Error allocating core memory buffers\"\nhandleExit 23 = \"Partial transfer due to error\"\nhandleExit 24 = \"Partial transfer due to vanished source files\"\nhandleExit 25 = \"The --max-delete limit stopped deletions\"\nhandleExit 30 = \"Timeout in data send/receive\"\nhandleExit 35 = \"Timeout waiting for daemon connection\"\n-}\n","","","")]
                   output
    where
      test :: (Module, [Comment]) -> String -> [(String, String, String, String)]
      test (m, comments) text =
          foldModule tailf pragmaf namef warningf tailf exportf tailf importf declf tailf m comments text []
          where
            pragmaf :: ModulePragma -> String -> String -> String -> [(String, String, String, String)] -> [(String, String, String, String)]
            pragmaf x pref s suff r = r ++ [(pref, s, suff,int (spanInfo x))]
            namef :: ModuleName -> String -> String -> String -> [(String, String, String, String)] -> [(String, String, String, String)]
            namef x pref s suff r = r ++ [(pref, s, suff,int (spanInfo x))]
            warningf :: WarningText -> String -> String -> String -> [(String, String, String, String)] -> [(String, String, String, String)]
            warningf x pref s suff r = r ++ [(pref, s, suff,int (spanInfo x))]
            exportf :: ExportSpec -> String -> String -> String -> [(String, String, String, String)] -> [(String, String, String, String)]
            exportf x pref s suff r = r ++ [(pref, s, suff,int (spanInfo x))]
            importf :: ImportDecl -> String -> String -> String -> [(String, String, String, String)] -> [(String, String, String, String)]
            importf x pref s suff r = r ++ [(pref, s, suff,int (spanInfo x))]
            declf :: Decl -> String -> String -> String -> [(String, String, String, String)] -> [(String, String, String, String)]
            declf x pref s suff r = r ++ [(pref, s, suff,int (spanInfo x))]
            tailf :: String -> [(String, String, String, String)] -> [(String, String, String, String)]
            tailf s r = r ++ [(s, "", "", "")]

int :: HasSpanInfo a => a -> String
int x = let (SrcSpanInfo (SrcSpan _ a b c d) _) = spanInfo x in "[" ++ show a ++ "." ++ show b ++ ":" ++ show c ++ "." ++ show d ++ "]"

test6 :: Test
test6 = TestCase (assertEqual "tree1"
                              (Node {rootLabel = sp 2 20,
                                     subForest = [Node {rootLabel = sp 5 10,
                                                        subForest = [Node {rootLabel = sp 6 7, subForest = []},
                                                                     Node {rootLabel = sp 8 9, subForest = []}]},
                                                  Node {rootLabel = sp 11 18,
                                                        subForest = [Node {rootLabel = sp 12 15, subForest = []}]}]})
                              (makeTree (fromList
                                         [sp 2 20,
                                          sp 5 10,
                                          sp 11 18,
                                          sp 6 7,
                                          sp 8 9,
                                          sp 12 15])))
    where
      sp a b = mkspan (29, a) (29, b)
      mkspan (a, b) (c, d) =
          SrcSpanInfo {srcInfoSpan = SrcSpan {srcSpanFilename = "<unknown>.hs",
                                              srcSpanStartLine = a,
                                              srcSpanStartColumn = b,
                                              srcSpanEndLine = c,
                                              srcSpanEndColumn = d}, srcInfoPoints = []}

test4 :: Test
test4 = TestCase (assertEqual "test4" (SrcLoc "<unknown>.hs" 2 24 < SrcLoc "<unknown>.hs" 29 7) True)
