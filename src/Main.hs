module Main (main) where

import           Control.Monad (unless)
import           System.Exit (exitFailure)
import           System.IO

import           Count
import           Parse
import           Options
import           Report
import qualified Interpreter

main :: IO ()
main = do
  (options, files) <- getOptions
  let ghciArgs = ghcOptions options ++ files

  -- get examples from Haddock comments
  modules <- getDocTests (ghcOptions options) files

  let c = countModules' modules
  hPutStrLn stderr (show c)

  if DumpOnly `elem` options
    then do
      -- dump to stdout
      print modules
    else do
      -- map to unit tests
      Interpreter.withInterpreter ghciArgs $ \repl -> do
        r <- runModules repl True modules
        unless (isSucceeded r) exitFailure
