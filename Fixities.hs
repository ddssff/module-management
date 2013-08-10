{-# LANGUAGE TemplateHaskell #-}
module Fixities where
import GetFixities
import FixitiesInst
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.Exts.Fixity (baseFixities)

do
    lf <- runIO $ getFixities ["Control.Lens"]
    fmap (:[]) $ valD (varP (mkName "fixities")) (normalB
            [| lf ++ baseFixities |]) []
