-- | 'foldModule' is a utility function used to implement the clean, split, and merge operations.
{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Modules.Fold
    ( ModuleInfo
    , ModuleMap
    , foldModule
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

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.State (get, put, runState, State)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.Default (Default(def))
import Data.List (tails)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>), Monoid)
import Data.Set.Extra as Set (fromList)
import Data.Tree (Tree(..))
import Language.Haskell.Exts.Annotated (ParseResult(..))
import qualified Language.Haskell.Exts.Annotated.Syntax as A (Decl, ExportSpec, ExportSpec(..), ExportSpecList(ExportSpecList), ImportDecl, Module(..), ModuleHead(..), ModuleName, ModulePragma, WarningText)
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), SrcSpanInfo(..))
import qualified Language.Haskell.Exts.Syntax as S (ModuleName)
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.Internal (parseFileWithComments, runMonadClean)
import Language.Haskell.Modules.Util.SrcLoc (endLoc, HasSpanInfo(..), increaseSrcLoc, makeTree, srcLoc, srcPairTextHead, srcPairTextTail, srcPairTextPair)
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

type ModuleInfo = (Module, String, [Comment])
type ModuleMap = Map S.ModuleName ModuleInfo

-- This happens, a span with end column 0, even though column
-- numbering begins at 1.  Is it a bug in haskell-src-exts?
fixSpan :: SrcSpanInfo -> SrcSpanInfo
fixSpan sp =
    if srcSpanEndColumn (srcInfoSpan sp) == 0
    then sp {srcInfoSpan = (srcInfoSpan sp) {srcSpanEndColumn = 1}}
    else sp

data St
    = St { loc_ :: SrcLoc
         , text_ :: String
         , comms_ :: [Comment]
         , sps_ :: [SrcSpanInfo] }
      deriving (Show)

setSpanEnd :: SrcLoc -> SrcSpan -> SrcSpan
setSpanEnd loc sp = sp {srcSpanEndLine = srcLine loc, srcSpanEndColumn = srcColumn loc}
setSpanStart :: SrcLoc -> SrcSpan -> SrcSpan
setSpanStart loc sp = sp {srcSpanStartLine = srcLine loc, srcSpanStartColumn = srcColumn loc}

-- | The spans returned by haskell-src-exts may put comments and
-- whitespace in the suffix string of a declaration, we want them in
-- the prefix string of the following declaration where possible.
adjustSpans :: String -> [Comment] -> [SrcSpanInfo] -> [SrcSpanInfo]
adjustSpans _ _ [] = []
adjustSpans _ _ [x] = [x]
adjustSpans text comments sps =
    fst $ runState f (St def text comments sps)
    where
      f = do st <- get
             let b = loc_ st
             case sps_ st of
               (ss1 : ssis) ->
                   do skip
                      st' <- get
                      let e = loc_ st'
                      case e >= endLoc ss1 of
                        True ->
                            -- We reached the end of ss1, so the segment from b to e is
                            -- trailing comments and space, some of which may belong in
                            -- the following span.
                            do put (st' {sps_ = ssis})
                               sps' <- f
                               let ss1' = ss1 {srcInfoSpan = setSpanEnd b (srcInfoSpan ss1)}
                               return (ss1' : sps')
                        False ->
                           -- If we weren't able to skip to the end of
                           -- the span, we encountered real text.
                           -- Move past one char and try again.
                           do case text_ st' of
                                "" -> return (sps_ st') -- error $ "Ran out of text\n st=" ++ show st ++ "\n st'=" ++ show st'
                                (c : t') -> do put (st' {text_ = t', loc_ = increaseSrcLoc [c] e})
                                               f
               sss -> return sss

      -- loc is the current position in the input file, text
      -- is the text starting at that location.
      skip :: State St ()
      skip = do loc1 <- loc_ <$> get
                skipWhite
                skipComment
                loc2 <- loc_ <$> get
                -- Repeat until failure
                when (loc1 /= loc2) skip

      skipWhite :: State St ()
      skipWhite = do st <- get
                     case span isSpace (text_ st) of
                       ("", _) -> return ()
                       (space, t') ->
                           let loc' = increaseSrcLoc space (loc_ st) in
                           put (st {loc_ = loc', text_ = t'})

      skipComment :: State St ()
      skipComment = do st <- get
                       case comms_ st of
                         (Comment _ csp _ : cs)
                             | srcLoc csp <= loc_ st ->
                                 -- We reached the comment, skip past it and discard
                                 case srcPairTextPair (loc_ st) (endLoc csp) (text_ st) of
                                   ("", _) -> return ()
                                   (comm, t') ->
                                       let loc' = increaseSrcLoc comm (loc_ st) in
                                       put (st {loc_ = loc',
                                                text_ = t',
                                                comms_ = cs})
                         _ -> return () -- No comments, or we didn't reach it

-- | Given the result of parseModuleWithComments and the original
-- module text, this does a fold over the parsed module contents,
-- calling the seven argument functions in order.  Each function is
-- passed the AST value, the text of the space and comments leading up
-- to the element, and the text for the element.  Note that not
-- everything passed to the "pre" argument of the functions will be
-- comments and space - for example, the "module" keyword will be
-- passed in the pre argument to the ModuleName function.
foldModule :: forall r. (Show r) =>
              (String -> r -> r) -- ^ Receives the space before the first pragma.
           -> (ModulePragma -> String -> String -> String -> r -> r) -- ^ Called once for each pragma.  In this and the similar arguments below, the three string arguments contain
                                                                     -- the comments and space preceding the construct, the text of the construct and the space following it.
           -> (ModuleName -> String -> String -> String -> r -> r) -- ^ Called with the module name.
           -> (WarningText -> String -> String -> String -> r -> r) -- ^ Called with the warning text between module name and export list
           -> (String -> r -> r) -- ^ Called with the export list open paren
           -> (ExportSpec -> String -> String -> String -> r -> r) -- ^ Called with each export specifier
           -> (String -> r -> r) -- ^ Called with the export list close paren and "where" keyword
           -> (ImportDecl -> String -> String -> String -> r -> r) -- ^ Called with each import declarator
           -> (Decl -> String -> String -> String -> r -> r) -- ^ Called with each top level declaration
           -> (String -> r -> r) -- ^ Called with comments following the last declaration
           -> ModuleInfo -- ^ Parsed module
           -> r -- ^ Fold initialization value
           -> r -- ^ Result
foldModule _ _ _ _ _ _ _ _ _ _ (A.XmlPage _ _ _ _ _ _ _, _, _) _ = error "XmlPage: unsupported"
foldModule _ _ _ _ _ _ _ _ _ _ (A.XmlHybrid _ _ _ _ _ _ _ _ _, _, _) _ = error "XmlHybrid: unsupported"
foldModule topf pragmaf namef warnf pref exportf postf importf declf sepf (m@(A.Module _ mh ps is ds), text, comments) r0 =
    (\ (_, (_, _, _, r)) -> r) $ runState doModule (text, def, spans m, r0)
    where
      doModule =
          do doSep topf
             doList pragmaf ps
             maybe (return ()) doHeader mh
             doList importf is
             (tl, l, sps, r) <- get
             put (tl, l, adjustSpans text comments sps, r)
             doList declf ds
             doTail sepf
      doHeader (A.ModuleHead sp n mw me) =
          do doItem namef n
             maybe (return ()) (doItem warnf) mw
             doSep pref
             maybe (return ()) (\ (A.ExportSpecList _ es) -> doList exportf es) me
             doClose postf sp
      doClose f sp =
          do (tl, l, sps, r) <- get
             case l < endLoc sp of
               True -> put (srcPairTextTail l (endLoc sp) tl,
                            endLoc sp,
                            sps,
                            f (srcPairTextHead l (endLoc sp) tl) r)
               False -> return ()
      doTail f =
          do (tl, l, sps, r) <- get
             put (tl, l, sps, f tl r)
      doSep :: (String -> r -> r) -> State (String, SrcLoc, [SrcSpanInfo], r) ()
      doSep f =
          do p <- get
             case p of
               (tl, l, sps@(sp : _), r) ->
                   do let l' = srcLoc sp
                      case l <= l' of
                        True ->
                            do put (srcPairTextTail l l' tl,
                                    l',
                                    sps,
                                    f (srcPairTextHead l l' tl) r)
                        False -> return ()
               _ -> error $ "foldModule - out of spans: " ++ show p
      doList :: (HasSpanInfo a, Show a) => (a -> String -> String -> String -> r -> r) -> [a] -> State (String, SrcLoc, [SrcSpanInfo], r) ()
      doList _ [] = return ()
      doList f (x : xs) = doItem f x >> doList f xs

      doItem :: (HasSpanInfo a, Show a) => (a -> String -> String -> String -> r -> r) -> a -> State (String, SrcLoc, [SrcSpanInfo], r) ()
      doItem f x =
          do (tl, l, (sp : sps'), r) <- get
             let -- Another haskell-src-exts bug?  If a module ends
                 -- with no newline, endLoc will be at the beginning
                 -- of the following (nonexistant) line.
                 pre = srcPairTextHead l (srcLoc sp) tl
                 tl' = srcPairTextTail l (srcLoc sp) tl
                 l' = endLoc sp
                 s = srcPairTextHead (srcLoc sp) l' tl'
                 tl'' = srcPairTextTail (srcLoc sp) l' tl'
                 l'' = adjust1 tl'' l'
                 post = srcPairTextHead l' l'' tl''
                 tl''' = srcPairTextTail l' l'' tl''
             put (tl''', l'', sps', f x pre s post r)

      -- Move to just past the last newline in the leading whitespace
      -- adjust "\n  \n  hello\n" (SrcLoc "<unknown>.hs" 5 5) ->
      --   (SrcLoc "<unknown>.hs" 7 1)
      _adjust :: String -> SrcLoc -> SrcLoc
      _adjust a l =
          l'
          where
            w = takeWhile isSpace a
            w' = take (length (takeWhile (elem '\n') (tails w))) w
            l' = increaseSrcLoc w' l

      -- Move to just past the first newline in the leading whitespace
      -- adjust "\n  \n  hello\n" (SrcLoc "<unknown>.hs" 5 5) ->
      --   (SrcLoc "<unknown>.hs" 6 1)
      adjust1 :: String -> SrcLoc -> SrcLoc
      adjust1 a l =
          l'
          where
            w = takeWhile isSpace a
            w' = case span (/= '\n') w of
                   (w'', '\n' : _) -> w'' ++ ['\n']
                   (w'', "") -> w''
            l' = increaseSrcLoc w' l

-- | Do just the header portion of 'foldModule'.
foldHeader :: forall r. (Show r) =>
              (String -> r -> r)
           -> (ModulePragma -> String -> String -> String -> r -> r)
           -> (ModuleName -> String -> String -> String -> r -> r)
           -> (WarningText -> String -> String -> String -> r -> r)
           -> ModuleInfo -> r -> r
foldHeader topf pragmaf namef warnf m r0 =
    foldModule topf pragmaf namef warnf ignore2 ignore ignore2 ignore ignore ignore2 m r0

-- | Do just the exports portion of 'foldModule'.
foldExports :: forall r. (Show r) =>
               (String -> r -> r)
            -> (ExportSpec -> String -> String -> String -> r -> r)
            -> (String -> r -> r)
            -> ModuleInfo -> r -> r
foldExports pref exportf postf m r0 =
    foldModule ignore2 ignore ignore ignore pref exportf postf ignore ignore ignore2 m r0

-- | Do just the imports portion of 'foldModule'.
foldImports :: forall r. (Show r) =>
               (ImportDecl -> String -> String -> String -> r -> r)
            -> ModuleInfo -> r -> r
foldImports importf m r0 =
    foldModule ignore2 ignore ignore ignore ignore2 ignore ignore2 importf ignore ignore2 m r0

-- | Do just the declarations portion of 'foldModule'.
foldDecls :: forall r. (Show r) =>
             (Decl -> String -> String -> String -> r -> r)
          -> (String -> r -> r)
          -> ModuleInfo -> r -> r
foldDecls declf sepf m r0 =
    foldModule ignore2 ignore ignore ignore ignore2 ignore ignore2 ignore declf sepf m r0

-- | This can be passed to foldModule to include the original text in the result
echo :: Monoid m => t -> m -> m -> m -> m -> m
echo _ pref s suff r = r <> pref <> s <> suff

-- | Similar to 'echo', but used for the two argument separator functions
echo2 :: Monoid m => m -> m -> m
echo2 s r = r <> s

-- | This can be passed to foldModule to omit the original text from the result.
ignore :: t -> m -> m -> m -> r -> r
ignore _ _ _ _ r = r

-- | Similar to 'ignore', but used for the two argument separator functions
ignore2 :: m -> r -> r
ignore2 _ r = r

tests :: Test
tests = TestLabel "Clean" (TestList [test1, test1b, test3, test4, test5, test6])

test1 :: Test
test1 =
    TestLabel "test1" $ TestCase $ withCurrentDirectory "testdata/original" $
    do let path = "Debian/Repo/Orphans.hs"
       text <- liftIO $ readFile path
       ParseOk (m, comments) <- runMonadClean $ parseFileWithComments path
       let (output, original) = test (m, text, comments)
       assertEqual "echo" original output
    where
      test :: ModuleInfo -> (String, String)
      test m@(_, text, _) = (foldModule echo2 echo echo echo echo2 echo echo2 echo echo echo2 m "", text)

test1b :: Test
test1b =
    TestLabel "test1b" $ TestCase $ withCurrentDirectory "testdata/original" $
    do let path = "Debian/Repo/Sync.hs"
       text <- liftIO $ readFile path
       ParseOk (m, comments) <- runMonadClean $ parseFileWithComments path
       let output = test (m, text, comments)
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
      test :: ModuleInfo -> [(String, String, String, String)]
      test m =
          foldModule tailf pragmaf namef warningf tailf exportf tailf importf declf tailf m []
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
       ParseOk (m, comments) <- runMonadClean $ parseFileWithComments path
       let (output, original) = test (m, text, comments)
       assertEqual "echo" original output
    where
      test :: ModuleInfo -> (String, String)
      test m@(_, text, _) = (foldModule echo2 echo echo echo echo2 echo echo2 echo echo echo2 m "", text)

test5 :: Test
test5 =
    TestLabel "test4" $ TestCase $ withCurrentDirectory "testdata/logic" $
    do let path = "Data/Logic/Classes/Literal.hs"
       text <- liftIO $ readFile path
       ParseOk (m, comments) <- runMonadClean $ parseFileWithComments path
       assertEqual "triples" expected (reverse $ foldDecls (\ _ a b c r -> (a, b, c) : r) (\ s r -> ("", s, "") : r) (m, text, comments) [])
    where
      expected =
          [("-- |Literals are the building blocks of the clause and implicative normal\n-- |forms.  They support negation and must include True and False elements.\n",
            "class (Negatable lit, Constants lit, HasFixity atom, Formula lit atom, Ord lit) => Literal lit atom | lit -> atom where\n    foldLiteral :: (lit -> r) -> (Bool -> r) -> (atom -> r) -> lit -> r\n\n",
            ""),
           ("","zipLiterals :: Literal lit atom =>\n               (lit -> lit -> Maybe r)\n            -> (Bool -> Bool -> Maybe r)\n            -> (atom -> atom -> Maybe r)\n            -> lit -> lit -> Maybe r","\n"),
           ("",
            "zipLiterals neg tf at fm1 fm2 =\n    foldLiteral neg' tf' at' fm1\n    where\n      neg' p1 = foldLiteral (neg p1) (\\ _ -> Nothing) (\\ _ -> Nothing) fm2\n      tf' x1 = foldLiteral (\\ _ -> Nothing) (tf x1) (\\ _ -> Nothing) fm2\n      at' a1 = foldLiteral (\\ _ -> Nothing) (\\ _ -> Nothing) (at a1) fm2",
            "\n"),
           (unlines ["",
                     "{- This makes bad things happen.",
                     "-- | We can use an fof type as a lit, but it must not use some constructs.",
                     "instance FirstOrderFormula fof atom v => Literal fof atom v where",
                     "    foldLiteral neg tf at fm = foldFirstOrder qu co tf at fm",
                     "        where qu = error \"instance Literal FirstOrderFormula\"",
                     "              co ((:~:) x) = neg x",
                     "              co _ = error \"instance Literal FirstOrderFormula\"",
                     "    atomic = Data.Logic.Classes.FirstOrder.atomic",
                     "-}",
                     "",
                     "-- |Just like Logic.FirstOrder.convertFOF except it rejects anything",
                     "-- with a construct unsupported in a normal logic formula,",
                     "-- i.e. quantifiers and formula combinators other than negation.",
                     ""],
            "fromFirstOrder :: forall formula atom v lit atom2.\n                  (Formula lit atom2, FOF.FirstOrderFormula formula atom v, Literal lit atom2) =>\n                  (atom -> atom2) -> formula -> Failing lit",
            "\n"),
           ("","fromFirstOrder ca formula =\n    FOF.foldFirstOrder (\\ _ _ _ -> Failure [\"fromFirstOrder\"]) co (Success . fromBool) (Success . atomic . ca) formula\n    where\n      co :: Combination formula -> Failing lit\n      co ((:~:) f) =  fromFirstOrder ca f >>= return . (.~.)\n      co _ = Failure [\"fromFirstOrder\"]\n\n",""),
           ("","fromLiteral :: forall lit atom v fof atom2. (Literal lit atom, FOF.FirstOrderFormula fof atom2 v) =>\n               (atom -> atom2) -> lit -> fof","\n"),
           ("","fromLiteral ca lit = foldLiteral (\\ p -> (.~.) (fromLiteral ca p)) fromBool (atomic . ca) lit","\n\n"),
           ("","toPropositional :: forall lit atom pf atom2. (Literal lit atom, P.PropositionalFormula pf atom2) =>\n                   (atom -> atom2) -> lit -> pf","\n"),
           ("","toPropositional ca lit = foldLiteral (\\ p -> (.~.) (toPropositional ca p)) fromBool (atomic . ca) lit","\n\n"),
           ("{-\nprettyLit :: forall lit atom term v p f. (Literal lit atom v, Apply atom p term, Term term v f) =>\n              (v -> Doc)\n           -> (p -> Doc)\n           -> (f -> Doc)\n           -> Int\n           -> lit\n           -> Doc\nprettyLit pv pp pf _prec lit =\n    foldLiteral neg tf at lit\n    where\n      neg :: lit -> Doc\n      neg x = if negated x then text {-\"\172\"-} \"~\" <> prettyLit pv pp pf 5 x else prettyLit pv pp pf 5 x\n      tf = text . ifElse \"true\" \"false\"\n      at = foldApply (\\ pr ts -> \n                        pp pr <> case ts of\n                                   [] -> empty\n                                   _ -> parens (hcat (intersperse (text \",\") (map (prettyTerm pv pf) ts))))\n                   (\\ x -> text $ if x then \"true\" else \"false\")\n      -- parensIf False = id\n      -- parensIf _ = parens . nest 1\n-}\n\n","prettyLit :: forall lit atom v. (Literal lit atom) =>\n              (Int -> atom -> Doc)\n           -> (v -> Doc)\n           -> Int\n           -> lit\n           -> Doc","\n"),
           ("","prettyLit pa pv pprec lit =\n    parensIf (pprec > prec) $ foldLiteral co tf at lit\n    where\n      co :: lit -> Doc\n      co x = if negated x then text {-\"\172\"-} \"~\" <> prettyLit pa pv 5 x else prettyLit pa pv 5 x\n      tf x = text (if x then \"true\" else \"false\")\n      at = pa 6\n      parensIf False = id\n      parensIf _ = parens . nest 1\n      Fixity prec _ = fixityLiteral lit\n\n",""),
           ("","fixityLiteral :: (Literal formula atom) => formula -> Fixity","\n"),
           ("","fixityLiteral formula =\n    foldLiteral neg tf at formula\n    where\n      neg _ = Fixity 5 InfixN\n      tf _ = Fixity 10 InfixN\n      at = fixity\n\n",""),
           ("","foldAtomsLiteral :: Literal lit atom => (r -> atom -> r) -> r -> lit -> r","\n"),
           ("","foldAtomsLiteral f i lit = foldLiteral (foldAtomsLiteral f i) (const i) (f i) lit","\n"),
           ("","","")]

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
