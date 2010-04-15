module Main where

import System.Environment (getArgs)
import Test.HUnit (runTestTT)
import DocTest (getTest)

main :: IO ()
main = do
  args  <- getArgs
  test  <- getTest args
  _     <- runTestTT test
  return ()
