{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Orphans where

import Data.Text (Text)
import qualified Debian.Control.Text as T

deriving instance Show (T.Field' Text)
deriving instance Ord (T.Field' Text)
deriving instance Show T.Paragraph
deriving instance Ord T.Paragraph
