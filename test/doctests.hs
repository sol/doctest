module Main where

import           Test.DocTest

main :: IO ()
main = doctest [
    "-packageghc"
  , "-isrc"
  , "-idist/build/autogen/"
  , "-optP-include"
  , "-optPdist/build/autogen/cabal_macros.h"
  , "src/Run.hs"
  ]
