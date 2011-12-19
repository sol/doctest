module Main where

import Test.HUnit (runTestTT, Test(..), Counts(..))
import System.Exit (exitSuccess, exitFailure)

import HaddockBackend.Api
import Options
import DocTest

import qualified Interpreter

main :: IO ()
main = do
  (options, files) <- getOptions
  let ghciArgs = ghcOptions options ++ files
  Interpreter.withInterpreter ghciArgs $ \repl -> do

    -- get examples from Haddock comments
    let haddockFlags = haddockOptions options
    docTests <- getDocTests haddockFlags files

    if DumpOnly `elem` options
      then do
        -- dump to stdout
        print docTests
      else do
        -- map to unit tests
        let tests = TestList $ map (toTestCase repl) docTests
        Counts _ _ e f <- runTestTT tests
        if e == 0 && f == 0
          then exitSuccess
          else exitFailure
