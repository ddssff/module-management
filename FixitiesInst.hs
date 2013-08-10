{-# LANGUAGE TemplateHaskell #-}
module FixitiesInst where
import GetFixities
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Syntax
import Language.Haskell.TH.Lift

deriveLiftMany [''Fixity, ''QName, ''Assoc, ''ModuleName, ''SpecialCon, ''Name, ''Boxed]
