{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Fold
    ( foldModule
    , foldHeader
    , foldExports
    , foldImports
    , foldDecls
    , echo
    , echo2
    , ignore
    , ignore2
    , tests
    ) where

import Control.Monad.State (State, runState, get, put)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.Default (Default(def))
import Data.List (tails)
import Data.Monoid (Monoid, (<>))
import Data.Set.Extra as Set (fromList)
import Data.Tree (Tree(..) {-, drawTree-})
import Language.Haskell.Exts.Annotated (ParseResult(..))
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl, ExportSpec, ExportSpecList(ExportSpecList), ImportDecl, ExportSpec(..), Module(..), ModuleHead(..), ModuleName, ModulePragma, WarningText)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Params (runCleanT, parseFile)
import Language.Haskell.Modules.Util.SrcLoc (HasSpanInfo(..), srcLoc, endLoc, makeTree, increaseSrcLoc, srcPairTextHead, srcPairTextTail)
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

instance Spans (A.ExportSpec SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]
instance Spans (A.ModulePragma SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]
instance Spans (A.ImportDecl SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]
instance Spans (A.Decl SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]
instance Spans (A.ModuleName SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]
instance Spans (A.WarningText SrcSpanInfo) where spans x = [fixSpan $ spanInfo x]

-- This happens.  Is it a bug in haskell-src-exts?
fixSpan :: SrcSpanInfo -> SrcSpanInfo
fixSpan sp =
    if srcSpanEndColumn (srcInfoSpan sp) == 0
    then sp {srcInfoSpan = (srcInfoSpan sp) {srcSpanEndColumn = 1}}
    else sp

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
           -> Module -> String -> r -> r
foldModule _ _ _ _ _ _ _ _ _ _ (A.XmlPage _ _ _ _ _ _ _) _ _ = error "XmlPage: unsupported"
foldModule _ _ _ _ _ _ _ _ _ _ (A.XmlHybrid _ _ _ _ _ _ _ _ _) _ _ = error "XmlHybrid: unsupported"
foldModule topf pragmaf namef warnf pref exportf postf importf declf sepf m@(A.Module _ mh ps is ds) text r0 =
    fst $ runState (doModule r0) (text, def, spans m)
    where
      doModule r =
          doSep topf r >>=
          doList pragmaf ps >>=
          maybe return doHeader mh >>=
          doList importf is >>=
          doList declf ds >>=
          doTail sepf
      doHeader (A.ModuleHead sp n mw me) r =
          doItem namef n r >>=
          maybe return (doItem warnf) mw >>=
          doSep pref >>=
          maybe return (\ (A.ExportSpecList _ es) -> doList exportf es) me >>=
          doClose postf sp
      doClose f sp r =
          do (tl, l, sps) <- get
             case l < endLoc sp of
               True -> do put (srcPairTextTail l (endLoc sp) tl, endLoc sp, sps)
                          return (f (srcPairTextHead l (endLoc sp) tl) r)
               False -> return r
      doTail f r =
          do (tl, _, _) <- get
             return $ f tl r
      doSep :: (String -> r -> r) -> r -> State (String, SrcLoc, [SrcSpanInfo]) r
      doSep f r =
          do p <- get
             case p of
               (tl, l, sps@(sp : _)) ->
                   do let l' = srcLoc sp
                      case l <= l' of
                        True ->
                            do put (srcPairTextTail l l' tl, l', sps)
                               return $ f (srcPairTextHead l l' tl) r
                        False -> return r
               _ -> error $ "foldModule - out of spans: " ++ show p
      doList :: (HasSpanInfo a, Show a) => (a -> String -> String -> String -> r -> r) -> [a] -> r -> State (String, SrcLoc, [SrcSpanInfo]) r
      doList _ [] r = return r
      doList f (x : xs) r = doItem f x r >>= doList f xs

      doItem :: (HasSpanInfo a, Show a) => (a -> String -> String -> String -> r -> r) -> a -> r -> State (String, SrcLoc, [SrcSpanInfo]) r
      doItem f x r =
          do (tl, l, (sp : sps')) <- get
             let -- Another haskell-src-exts bug?  If a module ends
                 -- with no newline, endLoc will be at the beginning
                 -- of the following (nonexistant) line.
                 l' = endLoc sp
                 pre = srcPairTextHead l (srcLoc sp) tl
                 tl' = srcPairTextTail l (srcLoc sp) tl
                 s = srcPairTextHead (srcLoc sp) l' tl'
                 tl'' = srcPairTextTail (srcLoc sp) l' tl'
                 l'' = adjust tl'' l'
                 post = srcPairTextHead l' l'' tl''
                 tl''' = srcPairTextTail l' l'' tl''
             put (tl''', l'', sps')
             return $ f x pre s post r

      -- Move to just past the first newline in the leading whitespace
      -- adjust "\n  \n  hello\n" (SrcLoc "<unknown>.hs" 5 5) ->
      --   (SrcLoc "<unknown>.hs" 6 1)
      _adjust2 :: String -> SrcLoc -> SrcLoc
      _adjust2 a l =
          l'
          where
            w = takeWhile isSpace a
            w' = case span (/= '\n') w of
                   (w'', '\n' : _) -> w'' ++ ['\n']
                   (w'', "") -> w''
            l' = increaseSrcLoc w' l

      -- Move to just past the last newline in the leading whitespace
      -- adjust "\n  \n  hello\n" (SrcLoc "<unknown>.hs" 5 5) ->
      --   (SrcLoc "<unknown>.hs" 7 1)
      adjust :: String -> SrcLoc -> SrcLoc
      adjust a l =
          l'
          where
            w = takeWhile isSpace a
            w' = take (length (takeWhile (elem '\n') (tails w))) w
            l' = increaseSrcLoc w' l

foldHeader :: forall r. (Show r) =>
              (String -> r -> r)
           -> (ModulePragma -> String -> String -> String -> r -> r)
           -> (ModuleName -> String -> String -> String -> r -> r)
           -> (WarningText -> String -> String -> String -> r -> r)
           -> Module -> String -> r -> r
foldHeader topf pragmaf namef warnf m text r0 =
    foldModule topf pragmaf namef warnf ignore2 ignore ignore2 ignore ignore ignore2 m text r0

foldExports :: forall r. (Show r) =>
               (String -> r -> r)
            -> (ExportSpec -> String -> String -> String -> r -> r)
            -> (String -> r -> r)
            -> Module -> String -> r -> r
foldExports pref exportf postf m text r0 =
    foldModule ignore2 ignore ignore ignore pref exportf postf ignore ignore ignore2 m text r0

foldImports :: forall r. (Show r) =>
               (ImportDecl -> String -> String -> String -> r -> r)
            -> Module -> String -> r -> r
foldImports importf m text r0 =
    foldModule ignore2 ignore ignore ignore ignore2 ignore ignore2 importf ignore ignore2 m text r0

foldDecls :: forall r. (Show r) =>
             (Decl -> String -> String -> String -> r -> r)
          -> (String -> r -> r)
          -> Module -> String -> r -> r
foldDecls declf sepf m text r0 =
    foldModule ignore2 ignore ignore ignore ignore2 ignore ignore2 ignore declf sepf m text r0

echo :: Monoid m => t -> m -> m -> m -> m -> m
echo _ pref s suff r = r <> pref <> s <> suff

echo2 :: Monoid m => m -> m -> m
echo2 s r = r <> s

ignore :: t -> m -> m -> m -> r -> r
ignore _ _ _ _ r = r

ignore2 :: m -> r -> r
ignore2 _ r = r

tests :: Test
tests = TestLabel "Clean" (TestList [test1, test1b, test3, test4, test6])

test1 :: Test
test1 =
    TestLabel "test1" $ TestCase $ withCurrentDirectory "testdata/original" $
    do let path = "Debian/Repo/Orphans.hs"
       text <- liftIO $ readFile path
       ParseOk m <- runCleanT $ parseFile path
       let (output, original) = test m text
       assertEqual "echo" original output
    where
      test :: Module -> String -> (String, String)
      test m text = (foldModule echo2 echo echo echo echo2 echo echo2 echo echo echo2 m text "", text)

test1b :: Test
test1b =
    TestLabel "test1b" $ TestCase $ withCurrentDirectory "testdata/original" $
    do let path = "Debian/Repo/Sync.hs"
       text <- liftIO $ readFile path
       ParseOk m <- runCleanT $ parseFile path
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
      test :: Module -> String -> [(String, String, String, String)]
      test m text =
          foldModule tailf pragmaf namef warningf tailf exportf tailf importf declf tailf m text []
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

test3 :: Test
test3 =
    TestLabel "test3" $ TestCase $ withCurrentDirectory "testdata" $
    do let path = "Equal.hs"
       text <- liftIO $ readFile path
       ParseOk m <- runCleanT $ parseFile path
       let (output, original) = test m text
       assertEqual "echo" original output
    where
      test :: Module -> String -> (String, String)
      test m text = (foldModule echo2 echo echo echo echo2 echo echo2 echo echo echo2 m text "", text)

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
