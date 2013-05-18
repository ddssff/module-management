{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
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

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, catch, throw, try)
import Control.Monad (foldM)
import Control.Monad.State (State, runState, get, put)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.Default (Default, def)
import Data.Function (on)
import Data.List (intercalate, sortBy, groupBy, partition)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Monoid ((<>))
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(..))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Parser (parseModule)
import Language.Haskell.Exts.Pretty (Pretty(..), defaultMode, PPHsMode(..), PPLayout(..), prettyPrintWithMode, prettyPrint)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), SrcSpan(..), srcSpanEnd, srcSpanStart, mkSrcSpan, mergeSrcSpan)
import Language.Haskell.Exts.Syntax (CName, ImportDecl(..), ImportSpec(..), Module(..), ModulePragma(..), WarningText(..), Name(..), Match(..), ModuleName(..), ExportSpec(..), Decl(..))
import Language.Haskell.Imports.Params (dryRun, MonadParams)
import Language.Haskell.Imports.SrcLoc (HasSrcLoc(..), HasEndLoc(..), srcSpan, textEndLoc, srcSpanTriple, srcLocPairTriple, srcSpanText, srcLocSucc, {-srcLocPred,-} srcSpanStart', srcSpanEnd', srcPairText, untabify, lines')
import System.Directory (removeFile, renameFile)
import System.IO (hPutStrLn, hFlush, stdout, stderr)
import System.IO.Error (isDoesNotExistError)
import Test.HUnit (Test(TestList, TestCase), assertEqual, runTestTT)
import Text.PrettyPrint (text)

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

-- data Module = Module SrcLoc ModuleName [ModulePragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]

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

data SrcUnion a
    = Head' a Module
    | Comment' a Comment
    | ImportDecl' a ImportDecl
    | Decl' a Decl
    -- | Space' String SrcSpan
    | Space' a String SrcLoc
    | Other' a String SrcLoc
    deriving Eq

instance Show (SrcUnion SrcSpan) where
    show (Comment' sp c) = "Comment' " ++ displaySpan sp ++ " (" ++ show c ++ ")"
    show (Other' sp s _) = "Other' " ++ displaySpan sp ++ "(" ++ show s ++ ")"
    show (Space' sp s _) = "Space' " ++ displaySpan sp ++ "(" ++ show s ++ ")"
    show (ImportDecl' sp x) = "ImportDecl' " ++ displaySpan sp
    show (Decl' sp x) = "Decl' " ++ displaySpan sp
    show (Head' sp x) = "Decl' " ++ displaySpan sp

class HasSrcSpan a where
    srcSpan' :: a -> SrcSpan

instance HasSrcSpan (SrcUnion SrcSpan) where
    srcSpan' (Comment' sp _) = sp
    srcSpan' (Other' sp _ _) = sp
    srcSpan' (Space' sp _ _) = sp
    srcSpan' (ImportDecl' sp _) = sp
    srcSpan' (Decl' sp _) = sp
    srcSpan' (Head' sp _) = sp

displaySpan :: SrcSpan -> String
displaySpan (SrcSpan _ bl bc el ec) = show bl ++ ":" ++ show bc ++ " - " ++ show el ++ ":" ++ show ec

instance Show (SrcUnion ()) where
    show (Comment' () c) = "Comment' " ++ show c
    show (Other' () s _) = "Other' " ++ show s
    show (Space' () s _) = "Space' " ++ show s
    show (ImportDecl' () x) = "ImportDecl' " ++ show x
    show (Decl' () x) = "Decl' " ++ show x
    show (Head' () x) = "Head' " ++ show x

getA :: SrcUnion a -> a
getA (Head' x _) = x
getA (Comment' x _) = x
getA (ImportDecl' x _) = x
getA (Decl' x _) = x
getA (Space' x _ _) = x
getA (Other' x _ _) = x

mapA :: (a -> b) -> SrcUnion a -> SrcUnion b
mapA f (Head' x y) = Head' (f x) y
mapA f (Comment' x y) = Comment' (f x) y
mapA f (ImportDecl' x y) = ImportDecl' (f x) y
mapA f (Decl' x y) = Decl' (f x) y
mapA f (Space' x y z) = Space' (f x) y z
mapA f (Other' x y z) = Other' (f x) y z

instance HasSrcLoc (SrcUnion a) where
    srcLoc (Head' _ x) = srcLoc x
    srcLoc (Comment' _ x) = srcLoc x
    srcLoc (ImportDecl' _ x) = srcLoc x
    srcLoc (Decl' _ x) = srcLoc x
    srcLoc (Space' _ _ l) = l
    srcLoc (Other' _ _ l) = l

instance HasEndLoc (SrcUnion SrcSpan) where
    endLoc = srcSpanEnd' . getA

data TextClass
    = CommentText String
    | Space String
    | Other String

{-
zipOverlappingItems :: [SrcLocUnion] -> [SrcLocUnion] -> ([SrcLocUnion], SrcLoc)
zipOverlappingItems (x : xs) (c : cs) =
-}

-- | Wrap the source code elements in SrcLocUnion constructors, sort, and add end locations
moduleDecls text m@(Module loc name pragmas warn exps imps decls) =
    case insertSpaceItems text (sortBy (compare `on` srcLoc) ([Head' () m] ++ map (ImportDecl' ()) imps ++ map (Decl' ()) decls)) of
      -- The first element may be a comment, which will also get returned by moduleSpace
      Other' {} : xs -> xs
      xs -> xs

-- | Same for comments, then group runs of Comment or Space, these are guaranteed to be adjacent
moduleSpace text comments =
    groupSpace text $ insertSpaceItems text $ sortBy (compare `on` srcLoc) (map (Comment' ()) comments)

-- | Zip the decls and space together sorted by end position.  Collect overlapping items into groups.
-- To collect overlaps, we first sort by endLoc and reverse the list.  As we scan this list, items whose start location precedes 
moduleItemGroups :: String -> Module -> [Comment] -> [[SrcUnion SrcSpan]]
moduleItemGroups text m comments =
    reverse $ groups
    where
      -- Given the items sorted in order of descending end location,
      -- we start a new group whenever we see an end location equal to
      -- the smallest start location yet seen
      groups =
          let (x : xs) = descendingEnds in
          loop (srcLoc x) (endLoc x) [[x]] xs
          where
            loop _ _ rss [] = rss
            loop b e (r : rs) (y : ys) =
                if endLoc y == e || srcLoc y >= b
                then loop (min b (srcLoc y)) (max e (endLoc y)) ((y : r) : rs) ys
                else loop (srcLoc y) (endLoc y) ([y] : (r : rs)) ys
{-
      groups =
          let (x : xs) = descendingEnds in
          loop (srcLoc x) [[x]] xs
              where loop loc (rs : rss) (x : ys) =
                        if srcLoc x > loc
                        then loop loc ((x : rs) : rss) ys
                        else loop loc [x] : (rs : rss)
            loop (rs : rss)
-}
{-
      groups = groupBy (\ a b -> srcLoc a < srcLoc b || endLoc a == endLoc b) $ descendingEnds
-}
      descendingEnds = reverse $ sortBy (compare `on` endLoc) $ decls ++ space
      decls = moduleDecls text m
      space = moduleSpace text comments

moduleItems text m comments =
    descendingEnds
    where
      descendingEnds = reverse $ sortBy (compare `on` endLoc) $ decls ++ space
      decls = moduleDecls text m
      space = moduleSpace text comments

toEq :: Ord a => (a -> a -> Ordering) -> (a -> a -> Bool)
toEq cmp a b =
    case cmp a b of
      EQ -> True
      _ -> False

groupBy' :: Ord a => (a -> a -> Ordering) -> [a] -> [[a]]
groupBy' cmp xs = groupBy (toEq cmp) $ sortBy cmp xs

filterEmbedded :: [SrcUnion SrcSpan] -> [SrcUnion SrcSpan]
filterEmbedded xs =
    filter (not . embedded sp) xs
    where
      sp = foldr1 mergeSrcSpan (map srcSpan' xs)
      spb = srcLoc sp
      spe = endLoc sp
      embedded sp x = srcLoc sp < srcLoc x && endLoc x < endLoc sp

-- | If two elements have the same end position, one will be space and
-- one will be code.  Move the end of the code element to just before
-- the beginning of the space element.  Finally, remove any space
-- items that start after their successor, these are embedded comments
-- which we can't do anything with.
moduleItemsFinal text m comments =
    sortBy (compare `on` srcSpan') $ concatMap (adjust . sortBy (compare `on` srcLoc)) groups
    where
      groups = map filterEmbedded (moduleItemGroups text m comments)
      -- Remove embedded spans from the list - those that begin after
      -- and end before the union of all the spans.  The resulting elements
      -- are then adjusted so they don't overlap.
      adjust xs =
          let sp = foldr1 mergeSrcSpan (map srcSpan' xs)
              spb = srcLoc sp
              spe = endLoc sp in
          case partition ((== sp) . srcSpan') xs of
            ([x], ys) ->
                case ys of
                  [y] ->
                      [mapA (const (srcSpan (srcLoc x) (srcLoc y))) x,
                       mapA (const (srcSpan (srcLoc y) (endLoc x))) y]
                  [] -> [x]
            ([], _) -> error "adjust: No covering element"
            _ -> error "adjust: multiple spanning elements"
{-
            [x] -> [x]
            [x, y] ->
                let bx = srcLoc x
                    by = srcLoc y
                    ex = endLoc x
                    ey = endLoc y in
                if bx == by
                then if ex == ey
                     then error "adjust: duplicate"
                     else [mapA (\ sp -> srcSpan by ex) y]
                else [mapA (\ sp -> srcSpan bx by) x,
                      mapA (\ sp -> srcSpan by ex) y]
            _ -> error $ "adjust: " ++ show xs ++ " -> " ++ show xs'
-}
{-
      filterEmbedded (x : y : zs) =
          if srcLoc (t1 y x) > srcLoc y
          then filterEmbedded (y : zs)
          else x : filterEmbedded (y : zs)
      filterEmbedded xs = xs
-}

-- Note that comments will overlap with the other elements
foldModule :: forall r.
              (Module -> String -> SrcSpan -> r -> r)
           -> (ImportDecl -> String -> SrcSpan -> r -> r) -- SrcLoc
           -> (Decl -> String -> SrcSpan -> r -> r) -- SrcLoc
           -> (Comment -> String -> r -> r) -- SrcSpan
           -> (String -> SrcSpan -> r -> r)
           -> (String -> SrcSpan -> r -> r)
           -> Module -> [Comment] -> String -> r -> r
foldModule headf importf declf commentf spacef otherf m@(Module loc name pragmas warn exps imps decls) comments text r0 =
    doItems (moduleItemsFinal text' m comments) r0
    where
      doItems (x : y : xs) r = doItems (y : xs) (doItem x (srcLoc x) (srcLoc y) r)
      doItems [x] r = doItem x (srcLoc x) (textEndLoc text) r
      doItems [] r = r
      doItem :: SrcUnion SrcSpan -> SrcLoc -> SrcLoc -> r -> r
      doItem (Comment' sp s) b e r = commentf s (srcPairText b e text) r
      doItem (Head' sp x) b e r = headf x (srcPairText b e text) sp r
      doItem (ImportDecl' sp x) b e r = importf x (srcPairText b e text) sp r
      doItem (Decl' sp x) b e r = declf x (srcPairText b e text) sp r
      doItem (Space' sp s l) b e r = spacef (srcPairText b e text) (srcSpan b e) r
      doItem (Other' sp s l) b e r = otherf (srcPairText b e text) (srcSpan b e) r
      text' = untabify text

-- | Given a list of Comment, Space and Other elements, discard the
-- Other elements, group adjoining elements, and then turn each group
-- of adjacent elements into a single Space element.
groupSpace :: String -> [SrcUnion SrcSpan] -> [SrcUnion SrcSpan]
groupSpace text xs =
    map makeSpace $ foldr f [[]] xs
    where
      -- Add an element to the newest list
      f x@(Space' {}) (xs : xss) = (x : xs) : xss
      f x@(Comment' {}) (xs : xss) = (x : xs) : xss
      -- Start a new list
      f x ([] : xss) = ([] : xss)
      f x (xs : xss) = ([] : xs : xss)
      -- Turn a list of elements into a single Space element
      makeSpace  :: [SrcUnion SrcSpan] -> SrcUnion SrcSpan
      makeSpace xs =
          let b = srcLoc (head xs)
              e = endLoc (last xs)
              sp = srcSpan b e in
          Space' sp (srcSpanText sp text) b

testGroupSpace =
    readFile "Debian/Repo/AptCache.hs" >>= \ text ->
    runTestTT $
    TestList
    [ TestCase (assertEqual "groupSpace"
                [Space' (SrcSpan "<unknown>.hs" 114 33 115 11) "-- Flip args to get newest first\n          " (SrcLoc "<unknown>.hs" 114 33)]
                (groupSpace
                   (untabify text)
                   [Comment' (SrcSpan "<unknown>.hs" 114 33 114 65)
                                 (Comment
                                  False
                                  (SrcSpan {srcSpanFilename = "<unknown>.hs",
                                            srcSpanStartLine = 114, srcSpanStartColumn = 33,
                                            srcSpanEndLine = 114, srcSpanEndColumn = 65})
                                  " Flip args to get newest first"),
                    Space' (SrcSpan "<unknown>.hs" 114 65 115 11) "\n          " (SrcLoc "<unknown>.hs" 114 65)])) ]

zipItems (x : xs) (y : ys) =
    case y of
      Comment' sp _ -> undefined
      Space' _ _ sp -> undefined
      Other' _ _ sp -> undefined
      _ -> error "zipItems"

insertSpaceItems :: String -> [SrcUnion ()] -> [SrcUnion SrcSpan]
insertSpaceItems text items =
    loop def items
    where
      loop :: SrcLoc -> [SrcUnion ()] -> [SrcUnion SrcSpan]
      loop loc [] = gap loc (textEndLoc text)
      loop loc (x : xs) | loc < srcLoc x = gap loc (srcLoc x) ++ loop (srcLoc x) (x : xs)
      loop loc (x : xs) =
          let end = next xs in
          case x of
            Comment' () c@(Comment _ sp _) ->
                Comment' sp c : loop (endLoc c) xs
            _ -> mapA (const (srcSpan loc end)) x : loop end xs
      gap :: SrcLoc -> SrcLoc -> [SrcUnion SrcSpan]
      gap b e =
          let s = srcPairText b e text
              sp = srcSpan b e in
          case span isSpace s of
            ("", "") -> []
            ("", _) -> [Other' sp s b]
            (_, "") -> [Space' sp s b]
            (s', t) ->
                let b' = appendLoc s' b in
                [Space' (srcSpan b b') s' b,
                 Other' (srcSpan b' (srcSpanEnd' sp)) t b']

      next :: [SrcUnion ()] -> SrcLoc
      next [] = textEndLoc text
      next (x : _) = srcLoc x

appendLoc :: String -> SrcLoc -> SrcLoc
appendLoc text loc =
    case lines' text of
      [line] -> loc {srcColumn = srcColumn loc + length line}
      xs -> loc {srcLine = srcLine loc + length xs - 1, srcColumn = length (last xs) + 1}

-- Classify the text segments by whether they are Comment, Space, or Other
classifyText :: [Comment] -> String -> [TextClass]
classifyText comments text =
    fst $ foldr f ([], def) comments
    where
      f :: Comment -> ([TextClass], SrcLoc) -> ([TextClass], SrcLoc)
      f c@(Comment _ sp _) (r, loc) =
          let r' = r ++ classify (srcPairText loc (commentStart c) text) ++ [CommentText (srcSpanText sp text)] in
          (r', srcLocSucc text (commentEnd c))
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
             return $ f m comments (untabify text')
         (Right text', Right _) -> error "parse failure"
         (Left (_ :: SomeException), _) -> error "failure"
         (_, Left (_ :: SomeException)) -> error "failure"

test1 :: IO ()
test1 =
    withTestData test1' >>= putStrLn
    where
      test1' :: Module -> [Comment] -> String -> String
      test1' m comments text =
          foldModule headf importf declf commentf spacef otherf m comments text ""
          where
            headf x s sp r = r ++ "head: " ++ displaySpan sp ++ "\n"
            importf x s sp r = r ++ "import: " ++ displaySpan sp ++ "\n"
            declf x s sp r = r ++ "decl: " ++ displaySpan sp ++ "\n"
            commentf x@(Comment _ sp _) s r = r ++ "comment: " ++ displaySpan sp ++ "\n"
            spacef s l r = r ++ "space: " ++ displaySpan l ++ "\n"
            otherf s l r = r ++ "other: " ++ displaySpan l ++ "\n"

test2a :: IO ()
test2a =
    withTestData test >>= hPutStrLn stderr . ("comments\n" ++)
    where
      test :: Module -> [Comment] -> String -> String
      test m comments text = unlines $ map show $ comments

test2b :: IO ()
test2b =
    withTestData test >>= hPutStrLn stderr . ("moduleSpace\n" ++)
    where
      test :: Module -> [Comment] -> String -> String
      test m@(Module loc name pragmas warn exps imps decls) comments text =
          unlines $ map show $ insertSpaceItems text $ sortBy (compare `on` srcLoc) (map (Comment' ()) comments)

test2c :: IO ()
test2c =
    withTestData test >>= hPutStrLn stderr . ("moduleDecls\n" ++)
    where
      test :: Module -> [Comment] -> String -> String
      test m@(Module loc name pragmas warn exps imps decls) comments text =
          unlines $ map show $ moduleDecls text m

test2d :: IO ()
test2d =
    withTestData test >>= hPutStrLn stderr . ("moduleItems\n" ++)
    where
      test :: Module -> [Comment] -> String -> String
      test m@(Module loc name pragmas warn exps imps decls) comments text =
          unlines $ map show $ moduleItems text m comments

test2e :: IO ()
test2e =
    withTestData test >>= hPutStrLn stderr . ("moduleItemGroups\n" ++)
    where
      test :: Module -> [Comment] -> String -> String
      test m@(Module loc name pragmas warn exps imps decls) comments text =
          unlines $ map show $ moduleItemGroups text m comments

test2f :: IO ()
test2f =
    withTestData test >>= hPutStrLn stderr . ("moduleItemGroups 2\n" ++)
    where
      test :: Module -> [Comment] -> String -> String
      test m@(Module loc name pragmas warn exps imps decls) comments text =
          unlines $ map show $ map filterEmbedded $ moduleItemGroups text m comments

test2g :: IO ()
test2g =
    withTestData test >>= hPutStrLn stderr . ("moduleItemsFinal\n" ++)
    where
      test :: Module -> [Comment] -> String -> String
      test m@(Module loc name pragmas warn exps imps decls) comments text =
          unlines $ map show $ moduleItemsFinal text m comments

test3 :: IO ()
test3 = putStrLn . show . textEndLoc $ "hello\nworld\n"

test4 :: IO ()
test4 =
    withTestData test >>= putStrLn
    where
      test :: Module -> [Comment] -> String -> String
      test m _ _ = show m

test5 :: IO ()
test5 =
    withTestData test >>= putStrLn
    where
      test :: Module -> [Comment] -> String -> String
      test _ cs _ = show cs
