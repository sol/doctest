module Main (main) where

import           Prelude
import           Test.DocTest
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= doctest
