module Main (main) where

import           Prelude
import qualified Test.DocTest.Internal.Cabal as Cabal
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= Cabal.doctest
