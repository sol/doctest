module Main where

import System.Environment (getArgs)
import Test.HUnit (runTestTT)
import DocTest (getTest)
import Options

main :: IO ()
main = do
  args  <- getArgs
  case parseOptions args of
    Right (options, files) -> do
      if (Help `elem` options)
        then do
          putStr usage
        else do
          test <- getTest options files
          _    <- runTestTT test
          return ()
    Left message -> do
      putStrLn message
