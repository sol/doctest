{-# LANGUAGE TemplateHaskell #-}
module Foo where

import Language.Haskell.TH
import Text.Printf

-- | Report an error.
--
-- >>> :set -fth
-- >>> $(logError "Something bad happened!")
-- ERROR <interactive>: Something bad happened!
logError :: String -> Q Exp
logError msg = do
  loc <- location
  let s = printf "ERROR %s: %s" (loc_filename loc) msg
  [| putStrLn s |]
