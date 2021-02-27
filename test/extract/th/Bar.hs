{-# LANGUAGE TemplateHaskell #-}

module Bar where

import Language.Haskell.TH.Lib (ExpQ)

bar :: ExpQ
bar = [| 23 |]
