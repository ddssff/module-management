{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Common
{-
    ( HasSrcLoc(srcLoc)
    , importsSpan
    , renameSpec
    , specName
    , replaceImports
    , tildeBackup
    , noBackup
    , replaceFile
    , foldModule
    , test1
    ) -} where

import Debug.Trace

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, catch, throw, try)
import Control.Monad (foldM)
import Control.Monad.State (State, runState, get, put)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.Default (Default, def)
import Data.Function (on)
import Data.List (intercalate, sortBy, groupBy)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Monoid ((<>))
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(..))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Parser (parseModule)
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(..), PPLayout(..), prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), srcSpanEnd, srcSpanStart, mkSrcSpan)
import Language.Haskell.Exts.Syntax (CName, ImportDecl(..), ImportSpec(..), Module(..), ModulePragma(..), WarningText(..), Name(..), Match(..), ModuleName(..), ExportSpec(..), Decl(..))
import Language.Haskell.Imports.Params (dryRun, MonadParams)
import System.Directory (removeFile, renameFile)
import System.IO (hPutStrLn, hFlush, stdout, stderr)
import System.IO.Error (isDoesNotExistError)

instance Default SrcLoc where
    def = SrcLoc def def def

-- | Class of values that contain a source location.
class HasSrcLoc x where
    srcLoc :: x -> SrcLoc

instance HasSrcLoc Module where
    srcLoc (Module s _ _ _ _ _ _) = s

instance HasSrcLoc Decl where
    srcLoc (TypeDecl s _ _ _) = s
    srcLoc (TypeFamDecl s _ _ _) = s
    srcLoc (DataDecl s _ _ _ _ _ _) = s
    srcLoc (GDataDecl s _ _ _ _ _ _ _) = s
    srcLoc (DataFamDecl s _ _ _ _) = s
    srcLoc (TypeInsDecl s _ _) = s
    srcLoc (DataInsDecl s _ _ _ _) = s
    srcLoc (GDataInsDecl s _ _ _ _ _) = s
    srcLoc (ClassDecl s _ _ _ _ _) = s
    srcLoc (InstDecl s _ _ _ _) = s
    srcLoc (DerivDecl s _ _ _) = s
    srcLoc (InfixDecl s _ _ _) = s
    srcLoc (DefaultDecl s _) = s
    srcLoc (SpliceDecl s _) = s
    srcLoc (TypeSig s _ _) = s
    srcLoc (FunBind (x:_)) = srcLoc x
    srcLoc (FunBind []) = error "srcLoc FunBind []"
    srcLoc (PatBind s _ _ _ _) = s
    srcLoc (ForImp s _ _ _ _ _) = s
    srcLoc (ForExp s _ _ _ _) = s
    srcLoc (RulePragmaDecl s _) = s
    srcLoc (DeprPragmaDecl s _) = s
    srcLoc (WarnPragmaDecl s _) = s
    srcLoc (InlineSig s _ _ _) = s
    srcLoc (InlineConlikeSig s _ _) = s
    srcLoc (SpecSig s _ _) = s
    srcLoc (SpecInlineSig s _ _ _ _) = s
    srcLoc (InstSig s _ _ _) = s
    srcLoc (AnnPragma s _) = s

instance HasSrcLoc Match where
    srcLoc (Match x _ _ _ _ _) = x

instance HasSrcLoc ImportDecl where
    srcLoc = importLoc

instance HasSrcLoc Comment where
    srcLoc (Comment _ sp _) = srcSpanStart' sp

-- | Class of values that contain a source location.
class HasEndLoc x where
    endLoc :: x -> SrcLoc

instance HasEndLoc Comment where
    endLoc (Comment _ sp _) = srcSpanEnd' sp

srcSpanStart' :: SrcSpan -> SrcLoc
srcSpanStart' sp = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanStart sp)

srcSpanEnd' :: SrcSpan -> SrcLoc
srcSpanEnd' sp = uncurry (SrcLoc (srcSpanFilename sp)) (srcSpanEnd sp)

cutSrcLoc :: SrcLoc -> String -> (String, String)
cutSrcLoc loc s =
    case splitAt (srcLine loc - 1) (lines s) of
      (h, t : ts) ->
          let (h', t') = splitAt (srcColumn loc - 1) t in
          (intercalate "\n" (h ++ [h']), unlines ([t'] ++ ts))
      (h, []) -> (intercalate "\n" h, "")

srcSpanTriple :: SrcSpan -> String -> (String, String, String)
srcSpanTriple sp s = srcLocPairTriple (srcSpanStart' sp) (srcSpanEnd' sp) s

srcLocPairTriple :: SrcLoc -> SrcLoc -> String -> (String, String, String)
srcLocPairTriple b e s =
    let (bmtext, etext) = cutSrcLoc e s
        (btext, mtext) = cutSrcLoc b bmtext in
    (btext, mtext, etext)

srcSpanText :: SrcSpan -> String -> String
srcSpanText sp s = let (_, m, _) = srcSpanTriple sp s in m

srcPairText :: SrcLoc -> SrcLoc -> String -> String
srcPairText b e s = let (_, m, _) = srcLocPairTriple b e s in m

srcLocSucc :: String -> SrcLoc -> Maybe SrcLoc
srcLocSucc text pos =
    case drop (srcLine pos - 1) (lines text) of
      [] -> Nothing
      (x : xs) -> case drop (srcColumn pos - 1) x of
                    "" -> case xs of
                            [] -> Nothing
                            _ -> Just (pos {srcLine = srcLine pos + 1, srcColumn = 1})
                    _ -> Just (pos {srcColumn = srcColumn pos + 1})

srcLocPred :: String -> SrcLoc -> Maybe SrcLoc
srcLocPred text pos =
    case srcColumn pos of
      n | n > 1 -> Just (pos {srcColumn = srcColumn pos - 1})
      _ -> case srcLine pos of
             n | n > 1 ->
                   -- Find the end of the previous line
                   let (line : _) = drop (srcLine pos - 2) (lines text) in
                   Just (pos {srcLine = srcLine pos - 1, srcColumn = length line {- - 1? -}})
             _ -> Nothing

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

-- | Change the symbol name (but not the module path) of an
-- ImportSpec.
renameSpec :: String -> ImportSpec -> ImportSpec
renameSpec s x = mapSpecName (const s) x

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
      -- Strip of the module declaration line, the leading spaces, and the terminating semicolons
      munge = unlines . map (init . tail . tail) . tail . lines

tildeBackup :: FilePath -> Maybe FilePath
tildeBackup = Just . (++ "~")

noBackup :: FilePath -> Maybe FilePath
noBackup = const Nothing

-- | Replace the file at path with the given text, moving the original
-- to the location returned by passing path to backup.  If backup is
-- the identity function you're going to have a bad time.
replaceFile :: MonadParams m => (FilePath -> Maybe FilePath) -> FilePath -> String -> m ()
replaceFile backup path text =
    dryRun >>= \ dryRun' ->
    case dryRun' of
      True -> liftIO $ putStrLn ("dryRun: replaceFile " ++ show path ++ " " ++ show text)
      False -> liftIO $ remove >> rename >> write
    where
      remove = maybe (return ()) removeFile (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      rename = maybe (return ()) (renameFile path) (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      write = writeFile path text

{-
data Module = Module SrcLoc ModuleName [ModulePragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]
-}

type ModuleState = (SrcLoc, [Comment])
type ModuleMonad = State ModuleState

getLoc :: ModuleMonad SrcLoc
getLoc = get >>= return . fst

putLoc :: SrcLoc -> ModuleMonad ()
putLoc loc =
    do (_, comments) <- get
       put (loc, comments)

lookComment :: ModuleMonad (Maybe Comment)
lookComment =
    do (_, comments) <- get
       case comments of
         [] -> return Nothing
         (x : _) -> return (Just x)

popComment :: ModuleMonad (Maybe Comment)
popComment =
    do (loc, comments) <- get
       case comments of
         [] -> return Nothing
         (x : xs) -> put (loc, xs) >> return (Just x)

{-
pushComment :: Comment -> ModuleMonad ()
pushComment comment =
    do (loc, comments) <- get
       put (loc, comment : comments)

takeComments :: (Comment -> Bool) -> ModuleMonad [Comment]
takeComments p =
    do (loc, comments) <- get
       case comments of
         (comment : comments') | p comment -> do put (loc, comments')
                                                 comments'' <- takeComments p
                                                 return $ comment : comments''
         _ -> return []

-- Because construct ends are not stored, we compute them by
-- finding the beginning of either the next command or the next
-- construct, whichever comes first.
nextCommentStart :: ModuleMonad (Maybe SrcLoc)
nextCommentStart = lookComment >>= return . fmap commentStart
-}

commentStart :: Comment -> SrcLoc
commentStart (Comment _ sp _) = srcSpanStart' sp

commentEnd :: Comment -> SrcLoc
commentEnd (Comment _ sp _) = srcSpanEnd' sp

-- Pop and return the comments that are embedded in the code before a
-- location, but not the ones that come after then end of the code.
embeddedComments :: SrcLoc -> ModuleMonad [Comment]
embeddedComments end =
    do (loc, comments) <- get
       let (candidates, later) = break (\ c -> commentEnd c >= end) comments
       let (embedded, between, _) = foldr (\ c (em, be, l) -> if commentEnd c == l
                                                              then (em, c : be, commentStart c)
                                                              else (c : em, be, l)) ([], [], end) (reverse candidates)
       put (loc, between ++ later)
       return embedded

-- [Head Text][Comment][Comment][Import1][Commment][Import2][Commment] ... [Comment][Decl1][Comment] ...

data SrcLocUnion
    = Module' Module
    | Comment' Comment
    | ImportDecl' ImportDecl
    | Decl' Decl
    | Space' String SrcLoc

-- With end position
data SrcLocUnion'
    = Module'' Module SrcLoc
    | Comment'' Comment
    | ImportDecl'' ImportDecl SrcLoc
    | Decl'' Decl SrcLoc
    | Space'' String SrcSpan

instance HasEndLoc SrcLocUnion' where
    endLoc (Module'' _ x) = x
    endLoc (Comment'' c) = endLoc c
    endLoc (ImportDecl'' _ x) = x
    endLoc (Decl'' _ x) = x
    endLoc (Space'' _ x) = srcSpanEnd' x

data TextClass
    = CommentText String
    | Space String
    | Other String

instance HasSrcLoc SrcLocUnion where
    srcLoc (Module' x) = srcLoc x
    srcLoc (Comment' x) = srcLoc x
    srcLoc (ImportDecl' x) = srcLoc x
    srcLoc (Decl' x) = srcLoc x

{-
zipOverlappingItems :: [SrcLocUnion] -> [SrcLocUnion] -> ([SrcLocUnion], SrcLoc)
zipOverlappingItems (x : xs) (c : cs) =
-}

-- Note that comments will overlap with the other elements
foldModule :: forall r.
              (Module -> String -> [Comment] -> r -> r)
           -> (ImportDecl -> String -> [Comment] -> r -> r) -- SrcLoc
           -> (Decl -> String -> [Comment] -> r -> r) -- SrcLoc
           -> (Comment -> String -> r -> r) -- SrcSpan
           -> Module -> [Comment] -> String -> r -> r
foldModule headf importf declf commentf m@(Module loc name pragmas warn exps imps decls) comments text r0 =
    doItems items r0
    where
{-
      items = sortBy (compare `on` srcLoc) ([Module' m] ++ map ImportDecl' imps ++ map Decl' decls)
      comments' = sortBy (compare `on` srcLoc) (map Comment' comments)
      items' = zipOverlappingItems items comments'

      items' = insertSpace
      -- Associate a SrcSpan each item.
      items' = 
-}
      -- Wrap the source code elements in SrcLocUnion constructors and sort
      items = insertSpaceItems (sortBy (compare `on` srcLoc) ([Module' m] ++ map ImportDecl' imps ++ map Decl' decls ++ map Comment' comments)) text
      doItems (x : y : xs) r = doItems (y : xs) (doItem x (srcLoc x) (srcLoc y) r)
      doItems [x] r = doItem x (srcLoc x) (end text) r
      doItems [] r = r
      doItem :: SrcLocUnion -> SrcLoc -> SrcLoc -> r -> r
      doItem (Comment' xs) b e r = commentf xs (srcPairText b e text) r
      doItem (Module' x) b e r = headf x (srcPairText b e text) undefined r
      doItem (ImportDecl' x) b e r = importf x (srcPairText b e text) undefined r
      doItem (Decl' x) b e r = declf x (srcPairText b e text) undefined r
      end :: String -> SrcLoc
      end text = def {srcLine = length (lines text), srcColumn = length (last (lines text))}
{-
      -- Group adjacent comments together
      comments' :: [[Comment]]
      comments' = groupBy (\ a b -> srcLocSucc text (commentEnd a) == Just (commentStart b)) $ comments
-}

insertSpaceItems :: [SrcLocUnion] -> String -> [SrcLocUnion]
insertSpaceItems items text =
    ins def items
    where
      ins :: SrcLoc -> [SrcLocUnion] -> [SrcLocUnion]
      ins _ [] = []
      ins loc (x : xs) =
          if loc < srcLoc x
          then Space' (srcPairText (srcLoc x) loc text) (srcLoc x) : ins (srcLoc x) (x : xs)
          else x : ins loc xs

-- Classify the text segments by whether they are Comment, Space, or Other
classifyText :: [Comment] -> String -> [TextClass]
classifyText comments text =
    fst $ foldr f ([], def) comments
    where
      f :: Comment -> ([TextClass], SrcLoc) -> ([TextClass], SrcLoc)
      f c@(Comment _ sp _) (r, loc) =
          let r' = r ++ classify (srcPairText loc (commentStart c) text) ++ [CommentText (srcSpanText sp text)] in
          case srcLocSucc text (commentEnd c) of
            Just e -> (r', e)
            Nothing -> (r', loc)
      classify "" = []
      classify s | all isSpace s = [Space s]
      classify s = [Other s]

withTestData :: (Module -> [Comment] -> String -> r) -> IO r
withTestData f =
    do let path = "Debian/Repo/AptCache.hs"
       text <- try (readFile path)
       source <- try (parseFileWithComments defaultParseMode path)
       case (text, source) of
         (Right text', Right (ParseOk (m, comments))) ->
             return $ f m comments text'
         (Left (_ :: SomeException), _) -> error "failure"
         (_, Left (_ :: SomeException)) -> error "failure"

test1 :: IO ()
test1 =
    withTestData test1' >>= putStrLn
    where
      test1' :: Module -> [Comment] -> String -> String
      test1' m comments text =
          foldModule headf importf declf commentf m comments text ""
          where
            headf x s _ r = r ++ show x ++ "\n"
            importf x s _ r = r ++ show x ++ "\n"
            declf x s _ r = r ++ show x ++ "\n"
            commentf x@(Comment _ sp _) s r = r ++ show x ++ "\n"

test2 :: IO ()
test2 =
    withTestData test2' >>= hPutStrLn stderr
    where
      test2' :: Module -> [Comment] -> String -> String
      test2' m comments text =
          let b = SrcLoc {srcFilename = "<unknown>.hs", srcLine = 40, srcColumn = 1}
              e = SrcLoc {srcFilename = "<unknown>.hs", srcLine = 41, srcColumn = 1} in
          -- trace (show text) text
          -- take 10 (show (drop 35 (lines text)))
          srcPairText (trace ("ib: " ++ show b) b) (trace ("ie: " ++ show e) e) text

{-
-- | March through the components and comments of a module in order of
-- the SrcLoc values.  We know the beginning location of most of the
-- module components, and the beginning and end of each comment.  The
-- fold functions are passed the module component, the text from the
-- module component's beginning until the beginning of the next module
-- component, and any comments that fall within 
foldModule' :: forall r.
              (Module -> String -> [Comment] -> r -> r)
           -> (ImportDecl -> String -> [Comment] -> r -> r) -- SrcLoc
           -> (Decl -> String -> [Comment] -> r -> r) -- SrcLoc
           -> (Comment -> r -> r) -- SrcSpan
           -> Module -> [Comment] -> String -> r -> r
foldModule' headf importf declf commentf m@(Module loc name pragmas warn exps imps decls) comments text r0 =
    -- Move through the module text alternately processing comments
    -- and module fields according to the source location.
    fst $ runState (doHead r0 >>= doImports >>= doDecls) (loc, comments)
    where

      end = loc {srcLine = length (lines text), srcColumn = length (last (lines text))}

      doHead :: r -> ModuleMonad r
      doHead r =
          do r' <- doCommentsBefore loc r
             cur <- getLoc -- the starting position
             -- Where does the first import start?
             let importLoc = fromMaybe end (listToMaybe (map srcLoc imps ++ map srcLoc decls))
             -- Get the comments that are embedded in the header code
             embedded <- embeddedComments importLoc
             -- Text ends at start of first import or next comment, whichever comes first
             headEnd <- lookComment >>= return . maybe importLoc (min importLoc . commentStart)
             return $ headf m (srcSpanText (mkSrcSpan cur headEnd) text) embedded r'

      doImports :: r -> ModuleMonad r
      doImports r = foldM doImport r imps
      doImport :: r -> ImportDecl -> ModuleMonad r
      doImport r imp =
          do r' <- doCommentsBefore (srcLoc imp) r
             return $ importf imp 
             return r'

      doDecls :: r -> ModuleMonad r
      doDecls r = foldM doDecl r decls
      doDecl :: r -> Decl -> ModuleMonad r
      doDecl r decl = return r

      -- Process comments until we reach loc
      doCommentsBefore :: SrcLoc -> r -> ModuleMonad r
      doCommentsBefore loc r =
          do (_, comments) <- get
             case comments of
               (comment : comments') | commentEnd comment <= loc -> popComment >> doComment comment r >>= doCommentsBefore loc
               _ -> return r
      doComment comment r =
          do putLoc (commentEnd comment)
             return $ commentf comment r
-}
