module Test.DocTest.Internal.Cabal (
  doctest
) where

import           Imports

import qualified Cabal

doctest :: [String] -> IO ()
doctest = Cabal.externalCommand
