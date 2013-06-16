{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.SourceBinaryNames
    ( sourceBinaryNames
    ) where


import Data.List as List (map)
import qualified Data.Text as T (unpack)
import Debian.Control (formatParagraph)
import qualified Debian.Control.Text as B (fieldValue, Paragraph)
import Debian.Relation (BinPkgName(..))
import Text.Regex (mkRegex, splitRegex)

sourceBinaryNames :: B.Paragraph -> [BinPkgName]
sourceBinaryNames paragraph =
    case B.fieldValue "Binary" paragraph of
      Just names -> List.map BinPkgName (splitRegex (mkRegex "[ ,\t\n]+") (T.unpack names))
      _ -> error ("Source package info has no 'Binary' field:\n" ++ (T.unpack . formatParagraph $ paragraph))


