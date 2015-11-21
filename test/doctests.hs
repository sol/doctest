module Main where

import           Test.DocTest

main :: IO ()
main = doctest $ [
    "--cpp"
  , "-packageghc"
  , "-isrc"
  , "-ighci-wrapper/src"
  , "src/Run.hs"
  , "src/PackageDBs.hs"
  ]
