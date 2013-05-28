{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
--- | A DebSource represents a release of a remote repository and a
--- method for accessing that repository.
module Debian.Repo.SourcesList
    (parseSourceLine,   -- String -> DebSource
     parseSourceLine',  -- String -> Maybe DebSource
     parseSourcesList,  -- String -> [DebSource]
     quoteWords -- String -> [String]
    )
    where

import Debian.Release (parseReleaseName, parseSection')
import Debian.Sources (DebSource(..), SourceType(..))
import Network.URI (parseURI, unEscapeString)

{-

deb uri distribution [component1] [componenent2] [...]

The URI for the deb type must specify the base of the Debian
distribution, from which APT will find the information it needs.

distribution can specify an exact path, in which case the components
must be omitted and distribution must end with a slash (/).

If distribution does not specify an exact path, at least one component
must be present.

Distribution may also contain a variable, $(ARCH), which expands to
the Debian architecture (i386, m68k, powerpc, ...)  used on the
system.

The rest of the line can be marked as a comment by using a #.

Additional Notes:

 + Lines can begin with leading white space.

 + If the dist ends with slash (/), then it must be an absolute path
   and it is an error to specify components after it.

-}

-- |quoteWords - similar to words, but with special handling of
-- double-quotes and brackets.
--
-- The handling double quotes and [] is supposed to match:
-- apt-0.6.44.2\/apt-pkg\/contrib\/strutl.cc:ParseQuoteWord()
--
-- The behaviour can be defined as:
--
--  Break the string into space seperated words ignoring spaces that
--  appear between \"\" or []. Strip trailing and leading white space
--  around words. Strip out double quotes, but leave the square
--  brackets intact.
quoteWords :: String -> [String]
quoteWords [] = []
quoteWords s = quoteWords' (dropWhile (==' ') s)
    where
      quoteWords' :: String -> [String]
      quoteWords' [] = []
      quoteWords' str =
          case break (flip elem " [\"") str of
            ([],[]) -> []
            (w, []) -> [w]
            (w, (' ':rest)) -> w : (quoteWords' (dropWhile (==' ') rest))
            (w, ('"':rest)) ->
                case break (== '"') rest of
                  (w',('"':rest)) ->
                      case quoteWords' rest of
                        [] ->  [w ++ w']
                        (w'':ws) -> ((w ++ w' ++ w''): ws)
                  (_w',[]) -> error ("quoteWords: missing \" in the string: "  ++ s)
                  _ -> error ("the impossible happened in SourcesList.quoteWords")
            (w, ('[':rest)) ->
                case break (== ']') rest of
                  (w',(']':rest)) ->
                      case quoteWords' rest of
                        []       -> [w ++ "[" ++ w' ++ "]"]
                        (w'':ws) -> ((w ++ "[" ++ w' ++ "]" ++ w''): ws)
                  (_w',[]) -> error ("quoteWords: missing ] in the string: "  ++ s)
                  _ -> error ("the impossible happened in SourcesList.quoteWords")
            _ -> error ("the impossible happened in SourcesList.quoteWords")

stripLine :: String -> String
stripLine = takeWhile (/= '#') . dropWhile (== ' ')

sourceLines :: String -> [String]
sourceLines = filter (not . null) . map stripLine . lines

-- |parseSourceLine -- parses a source line
-- the argument must be a non-empty, valid source line with comments stripped
-- see: 'sourceLines'
parseSourceLine :: String -> DebSource
parseSourceLine str =
    case quoteWords str of
      (theTypeStr : theUriStr : theDistStr : sectionStrs) ->
          let sections = map parseSection' sectionStrs
              theType = case unEscapeString theTypeStr of
                          "deb" -> Deb
                          "deb-src" -> DebSrc
                          o -> error ("parseSourceLine: invalid type " ++ o ++ " in line:\n" ++ str)
              theUri = case parseURI theUriStr of
                         Nothing -> error ("parseSourceLine: invalid uri " ++ theUriStr ++ " in the line:\n" ++ str)
                         Just u -> u
              theDist = unEscapeString theDistStr
          in
            case last theDist of
              '/' -> if null sections
                      then DebSource { sourceType = theType, sourceUri = theUri, sourceDist = Left theDist }
                      else error ("parseSourceLine: Dist is an exact path, so sections are not allowed on the line:\n" ++ str)
              _ -> if null sections
                    then error ("parseSourceLine: Dist is not an exact path, so at least one section is required on the line:\n" ++ str)
                    else DebSource { sourceType = theType, sourceUri = theUri, sourceDist = Right (parseReleaseName theDist, sections) }
      _ -> error ("parseSourceLine: invalid line in sources.list:\n" ++ str)

parseSourceLine' :: String -> Maybe DebSource
parseSourceLine' str =
    case quoteWords str of
      (theTypeStr : theUriStr : theDistStr : sectionStrs) ->
          let sections = map parseSection' sectionStrs
              theType = case unEscapeString theTypeStr of
                          "deb" -> Just Deb
                          "deb-src" -> Just DebSrc
                          _ -> Nothing
              theUri = case parseURI theUriStr of
                         Nothing -> Nothing
                         Just u -> Just u
              theDist = unEscapeString theDistStr
          in
            case (last theDist, theType, theUri) of
              ('/', Just typ, Just uri) -> if null sections
                      then Just $ DebSource { sourceType = typ, sourceUri = uri, sourceDist = Left theDist }
                      else error ("parseSourceLine: Dist is an exact path, so sections are not allowed on the line:\n" ++ str)
              (_, Just typ, Just uri) -> if null sections
                    then Nothing
                    else Just $ DebSource { sourceType = typ, sourceUri = uri, sourceDist = Right ((parseReleaseName theDist), sections) }
              _ -> Nothing
      _ -> Nothing

parseSourcesList :: String -> [DebSource]
parseSourcesList = map parseSourceLine . sourceLines
