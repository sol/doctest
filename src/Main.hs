module Main (main) where

import Test.HUnit (runTestTT, Test(..), Counts(..))
import System.Exit (exitSuccess, exitFailure)
import System.IO
import Data.Monoid

import Parse
import Options
import DocTest

import qualified Interpreter

main :: IO ()
main = do
  (options, files) <- getOptions
  let ghciArgs = ghcOptions options ++ files

  -- get examples from Haddock comments
  modules <- getDocTests (ghcOptions options) files

  let c = (mconcat . map count) modules
  hPutStrLn stderr (show c)

  if DumpOnly `elem` options
    then do
      -- dump to stdout
      print modules
    else do
      -- map to unit tests
      Interpreter.withInterpreter ghciArgs $ \repl -> do
        let tests = TestList $ map (toTestCase repl) modules
        Counts _ _ errCount failCount <- runTestTT tests
        if errCount == 0 && failCount == 0
          then exitSuccess
          else exitFailure

-- | Number of examples and interactions.
data Count = Count Int {- example count -} Int {- interaction count -}

instance Monoid Count where
  mempty = Count 0 0
  (Count x1 y1) `mappend` (Count x2 y2) = Count (x1 + x2) (y1 + y2)

instance Show Count where
  show (Count 1 1)           = "There is one test, with one single interaction."
  show (Count 1 iCount)      = "There is one test, with " ++ show iCount ++ " interactions."
  show (Count tCount iCount) = "There are " ++ show tCount ++ " tests, with " ++ show iCount ++ " total interactions."

-- | Count number of examples and interactions in fiven module.
count :: Module DocTest -> Count
count (Module _ examples) = (mconcat . map f) examples
  where
    f :: DocTest -> Count
    f (DocExample x) = Count 1 (length x)
