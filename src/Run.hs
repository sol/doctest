{-# LANGUAGE CPP #-}
module Run (
  doctest
#ifdef TEST
, doctest_
, Summary
, stripOptGhc
#endif
) where

import           Data.Monoid
import           Data.List
import           Data.Maybe
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
  case args of
    ["--help"] -> do
      putStr usage
    ["--version"] ->
      printVersion
    _ -> do
      r <- doctest_ (stripOptGhc args)
      when (not $ isSuccess r) exitFailure

-- | Strip --optghc from GHC options.  This is for backward compatibility with
-- previous versions of doctest.
stripOptGhc :: [String] -> [String]
stripOptGhc = go
  where
    go args = case args of
      []                      -> []
      "--optghc" : opt : rest -> opt : go rest
      opt : rest              -> fromMaybe opt (stripPrefix "--optghc=" opt) : go rest

doctest_ :: [String] -> IO Summary
doctest_ args = do

  -- get examples from Haddock comments
  modules <- getDocTests args

  let c = (mconcat . map count) modules
  hPrint stderr c

  Interpreter.withInterpreter args $ \repl -> do
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
