module Deriving where

import Data.Text (Text)
import Debian.Control (Paragraph(..), Paragraph'(..), Field'(..))

deriving instance Show (Field' String)
deriving instance Show Paragraph
