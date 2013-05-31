{-# LANGUAGE PackageImports, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Debian.Repo.Package.TryParseRel
    ( -- * Source and binary packages
      tryParseRel
    -- * Deprecated stuff for interfacing with Debian.Relation
    ) where

import qualified Debian.Control.Text as B (Field, Field'(Field))
import qualified Debian.Relation.Text as B (ParseRelations(..), Relations)

tryParseRel :: Maybe B.Field -> B.Relations
tryParseRel (Just (B.Field (_, relStr))) = either (error . show) id (B.parseRelations relStr)
tryParseRel _ = []
tryParseRel (Just (B.Field (_, relStr))) = either (error . show) id (B.parseRelations relStr)
tryParseRel _ = []