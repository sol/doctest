module Main where

import Test.HUnit (runTestTT)
import DocTest (getTest)
import Options (getOptions)

main :: IO ()
main = do
  (options, modules) <- getOptions
  test <- getTest options modules
  _    <- runTestTT test
  return ()
