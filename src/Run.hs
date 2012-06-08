{-# LANGUAGE CPP #-}
module Run (
  doctest
#ifdef TEST
, doctest_
, Summary
#endif
) where
import           Data.Monoid
import           Control.Monad (when)
import           System.Exit (exitFailure)
import           System.IO

import           Parse
import           Options
import           Report
import qualified Interpreter

-- | Run doctest with given list of arguments.
--
-- Example:
--
-- >>> doctest ["--optghc=-iexample/src", "example/src/Example.hs"]
-- There are 2 tests, with 2 total interactions.
-- Examples: 2  Tried: 2  Errors: 0  Failures: 0
--
-- This can be used to create a Cabal test suite that runs doctest for your
-- project.
doctest :: [String] -> IO ()
doctest args = do
  r <- doctest_ args
  when (not . isSuccess $ r) exitFailure

doctest_ :: [String] -> IO Summary
doctest_ args = do
  (options, files) <- parseOptions args
  let ghciArgs = ghcOptions options ++ files

  -- get examples from Haddock comments
  modules <- getDocTests (ghcOptions options) files

  let c = (mconcat . map count) modules
  hPrint stderr c

  if DumpOnly `elem` options
    then do
      -- dump to stdout
      print modules
      return mempty
    else do
      -- run tests
      Interpreter.withInterpreter ghciArgs $ \repl -> do
        runModules (exampleCount c) repl modules
  where
    exampleCount (Count n _) = n

isSuccess :: Summary -> Bool
isSuccess s = sErrors s == 0 && sFailures s == 0

-- | Number of examples and interactions.
data Count = Count Int {- example count -} Int {- interaction count -}

instance Monoid Count where
  mempty = Count 0 0
  (Count x1 y1) `mappend` (Count x2 y2) = Count (x1 + x2) (y1 + y2)

instance Show Count where
  show (Count 1 1)           = "There is one test, with one single interaction."
  show (Count 1 iCount)      = "There is one test, with " ++ show iCount ++ " interactions."
  show (Count tCount iCount) = "There are " ++ show tCount ++ " tests, with " ++ show iCount ++ " total interactions."

-- | Count number of examples and interactions in given module.
count :: Module DocTest -> Count
count (Module _ examples) = (mconcat . map f) examples
  where
    f :: DocTest -> Count
    f (Example x)  = Count 1 (length x)
    f (Property _) = Count 1 1
