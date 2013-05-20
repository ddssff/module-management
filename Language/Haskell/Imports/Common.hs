{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.Imports.Common
    ( Display(..)
    , HasSrcLoc(..)
    , HasSrcSpan(..)
    -- , importsSpan
    -- , renameSpec
    -- , specName
    -- , replaceImports
    , tildeBackup
    , noBackup
    , replaceFile
    , groupBy'
    , withCurrentDirectory
    , untabify
    , lines'
    ) where

import Control.Exception (catch, SomeException, throw, try)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B (readFile)
import Data.Char (isSpace)
import Data.Default (def, Default)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.List (groupBy, partition, sortBy)
import Data.Monoid ((<>))
import Language.Haskell.Exts (defaultParseMode, parseFileWithComments, ParseResult(..))
import Language.Haskell.Exts.Comments (Comment(..))
import Language.Haskell.Exts.Parser (parseModule)
import Language.Haskell.Exts.Pretty (defaultMode, PPHsMode(layout), PPLayout(PPInLine), prettyPrintWithMode)
import Language.Haskell.Exts.SrcLoc (mergeSrcSpan, mkSrcSpan, SrcSpan(..))
import Language.Haskell.Exts.Syntax (CName, Decl, ImportDecl, ImportSpec(..), Module(..), Name(..), SrcLoc(..))
import System.Directory (removeFile, renameFile, getCurrentDirectory, setCurrentDirectory)
import System.IO.Error (isDoesNotExistError)
import Test.HUnit (assertEqual, Test(TestCase, TestList))

{-
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
-}

tildeBackup :: FilePath -> Maybe FilePath
tildeBackup = Just . (++ "~")

noBackup :: FilePath -> Maybe FilePath
noBackup = const Nothing

{-
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
-}

-- | Replace the file at path with the given text, moving the original
-- to the location returned by passing path to backup.  If backup is
-- the identity function you're going to have a bad time.
replaceFile :: Bool -> (FilePath -> Maybe FilePath) -> FilePath -> String -> IO ()
replaceFile dryRun' backup path text =
    case dryRun' of
      True -> liftIO $ putStrLn ("dryRun: replaceFile " ++ show path ++ " " ++ show text)
      False -> liftIO $ remove >> rename >> write
    where
      remove = maybe (return ()) removeFile (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      rename = maybe (return ()) (renameFile path) (backup path) `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return () else throw e)
      write = writeFile path text

class Display a where
    display :: a -> String

class HasSrcSpan a where
    srcSpan' :: a -> SrcSpan

instance Display Comment where
    display (Comment _ sp _) = "Comment " ++ display sp

instance Display ImportDecl where
    display _ = "ImportDecl"

instance Display Decl where
    display _ = "Decl"

instance Display Module where
    display _ = "Module"

instance Display SrcSpan where
    display (SrcSpan _ bl bc el ec) = "[" ++ show bl ++ ":" ++ show bc ++ " - " ++ show el ++ ":" ++ show ec ++ "]"

instance Display SrcLoc where
    display (SrcLoc _ b e) = "[" ++ show b ++ ":" ++ show e ++ "]"

instance Default SrcLoc where
    def = SrcLoc "<unknown>.hs" 1 1

-- | Class of values that contain a source location.
class HasSrcLoc x where
    srcLoc :: x -> SrcLoc

instance HasSrcLoc SrcSpan where
    srcLoc (SrcSpan f b e _ _) = SrcLoc f b e

instance HasSrcLoc Module where
    srcLoc (Module s _ _ _ _ _ _) = s
{-
-- | Wrap the source code elements in SrcLocUnion constructors, sort, and add end locations
moduleDecls :: String -> Module -> [SrcUnion SrcSpan]
moduleDecls text m@(Module _ _ _ _ _ imps decls) =
    case insertSpaceItems text (sortBy (compare `on` srcLoc) ([Head' () m] ++ map (ImportDecl' ()) imps ++ map (Decl' ()) decls)) of
      -- The first element may be a comment, which will also get returned by moduleSpace
      Other' {} : xs -> xs
      xs -> xs

-- | Same for comments, then group runs of Comment or Space, these are guaranteed to be adjacent
moduleSpace :: String -> [Comment] -> [SrcUnion SrcSpan]
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
      descendingEnds = reverse $ sortBy (compare `on` endLoc) $ decls ++ space
      decls = moduleDecls text m
      space = moduleSpace text comments

moduleItems :: String -> Module -> [Comment] -> [SrcUnion SrcSpan]
moduleItems text m comments =
    descendingEnds
    where
      descendingEnds = reverse $ sortBy (compare `on` endLoc) $ decls ++ space
      decls = moduleDecls text m
      space = moduleSpace text comments

filterEmbedded :: [SrcUnion SrcSpan] -> [SrcUnion SrcSpan]
filterEmbedded xs =
    filter (not . embedded) xs
    where
      embedded x = srcLoc sp < srcLoc x && endLoc x < endLoc sp
      sp = foldr1 mergeSrcSpan (map srcSpan' xs)

-- | If two elements have the same end position, one will be space and
-- one will be code.  Move the end of the code element to just before
-- the beginning of the space element.  Finally, remove any space
-- items that start after their successor, these are embedded comments
-- which we can't do anything with.
moduleItemsFinal :: String -> Module -> [Comment] -> [SrcUnion SrcSpan]
moduleItemsFinal text m comments =
    sortBy (compare `on` srcSpan') $ concatMap (adjust . sortBy (compare `on` srcLoc)) groups
    where
      -- Remove embedded spans from the list - those that begin after
      -- and end before the union of all the spans.
      groups = map filterEmbedded (moduleItemGroups text m comments)
      -- Adjust the remaining elements so they don't overlap.
      adjust xs =
          case partition ((== sp) . srcSpan') xs of
            ([x], ys) ->
                case ys of
                  [y] ->
                      [mapA (const (srcSpan (srcLoc x) (srcLoc y))) x,
                       mapA (const (srcSpan (srcLoc y) (endLoc x))) y]
                  [] -> [x]
            ([], _) -> error "adjust: No covering element"
            _ -> error "adjust: multiple spanning elements"
          where
            sp = foldr1 mergeSrcSpan (map srcSpan' xs)

-- | Given a list of Comment, Space and Other elements, discard the
-- Other elements, group adjoining elements, and then turn each group
-- of adjacent elements into a single Space element.
groupSpace :: String -> [SrcUnion SrcSpan] -> [SrcUnion SrcSpan]
groupSpace text items =
    map makeSpace $ foldr f [[]] items
    where
      -- Add an element to the newest list
      f x@(Space' {}) (xs : xss) = (x : xs) : xss
      f x@(Comment' {}) (xs : xss) = (x : xs) : xss
      -- Start a new list
      f _ ([] : xss) = ([] : xss)
      f _ (xs : xss) = ([] : xs : xss)
      -- Turn a list of elements into a single Space element
      makeSpace  :: [SrcUnion SrcSpan] -> SrcUnion SrcSpan
      makeSpace xs =
          let b = srcLoc (head xs)
              e = endLoc (last xs)
              sp = srcSpan b e in
          Space' sp (srcSpanText sp text) b

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
-}

appendLoc :: String -> SrcLoc -> SrcLoc
appendLoc text loc =
    case lines' text of
      [line] -> loc {srcColumn = srcColumn loc + length line}
      xs -> loc {srcLine = srcLine loc + length xs - 1, srcColumn = length (last xs) + 1}

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path action =
    do save <- getCurrentDirectory
       setCurrentDirectory path
       result <- action
       setCurrentDirectory save
       return result

untabify :: String -> String
untabify s =
    loop 0 s
    where
      loop :: Int -> String -> String
      loop n ('\t' : s') = replicate (8 - mod n 8) ' ' ++ loop 0 s'
      loop _ ('\n' : s') = '\n' : loop 0 s'
      loop n (c : s') = c : loop (n + 1) s'
      loop _ [] = []

-- | Convert a compare function into an (==)
toEq :: Ord a => (a -> a -> Ordering) -> (a -> a -> Bool)
toEq cmp a b =
    case cmp a b of
      EQ -> True
      _ -> False

-- | Combine sortBy and groupBy
groupBy' :: Ord a => (a -> a -> Ordering) -> [a] -> [[a]]
groupBy' cmp xs = groupBy (toEq cmp) $ sortBy cmp xs

-- | A version of lines that preserves the presence or absence of a
-- terminating newline
lines' :: String -> [String]
lines' s =
    bol (groupBy (\ a b -> a /= '\n' && b /= '\n') s)
    where
      -- If we are at beginning of line and see a newline, insert an empty
      bol ("\n" : xs) = "" : bol xs
      -- If we are at beginning of line and see something else, call end of line
      bol (x : xs) = x : eol xs
      -- If we see EOF at bol insert a trailing empty
      bol [] = [""]
      -- If we are at end of line and see a newline, go to beginning of line
      eol ("\n" : xs) = bol xs
      -- This shouldn't happen
      eol (x : xs) = x : eol xs
      eol [] = []
