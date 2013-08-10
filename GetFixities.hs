module GetFixities where

import System.Process
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser
import Data.Maybe
import Control.Applicative
import Data.List
import Data.Char

getFixities :: [String] -> IO [Fixity]
getFixities modules = do
    r <- readProcess "ghc" (concatMap (\m ->[ "-e",":browse " ++ m ++ ""]) modules) ""
    let ops = mapMaybe (\x -> takeWhile (/=')') <$> stripPrefix "(" x) $ lines r
    fixes <- readProcess "ghc" (concatMap (\op -> ["-e",":info ("++op++")"]) ops) ""
    return $ concatMap viaOther $ lines fixes


viaExts s =  case parse s of
    ParseOk (InfixDecl _ assoc n ops) ->  map (Fixity assoc n . opToQn) ops
    _ -> []

viaOther s | Just s <- stripPrefix "infix" s =
    let a = case take 1 s of
            "r" -> AssocRight
            "l" -> AssocLeft
            _ -> AssocNone
        b = words (drop 1 s)

        (n,b') | [(n, "")] : _ <- map reads b = (n, drop 1 b)
                | otherwise = (9, b)
        toName ('`':s) | _:_ <-  s = case splitQual (init s) of
                            (s, "") -> UnQual (Ident s)
                            (s, m) -> Qual (ModuleName m) (Ident s)

        toName s = case splitQual s of
                (s, "") -> UnQual (Symbol s)
                (s, m) -> Qual (ModuleName m) (Symbol s)

        unq (Qual _ x) = UnQual x
        unq x = x

        splitQual xs = let (a,b) = span (\(n,c) -> n == 1 || c/='.') $ [1 .. ] `zip` reverse xs
                in (reverse (map snd a), map snd $ reverse (drop 1 b))

    in map (Fixity a n . unq . toName) b'
viaOther _ = []

opToQn :: Op -> QName
opToQn (VarOp n) = UnQual n
opToQn (ConOp n) = UnQual n
