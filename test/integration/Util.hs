module Util (
    doctestTestCase
  , Util.cases
  , errors
  , failures
  ) where

import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)
import qualified Test.HUnit as HU
import Test.HUnit (assertEqual, Counts(..), Test(TestCase), Assertion,
                   showCounts)
import Data.Char (isSpace)

cases :: Int -> Counts
cases n = Counts {
    HU.cases = n
  , tried    = n
  , errors   = 0
  , failures = 0
  }


-- | Construct a doctest specific 'TestCase'.
doctestTestCase :: FilePath -- ^ absolute path to `doctest` binary
                -> FilePath -- ^ current directory of forked `doctest` process
                -> [String] -- ^ args, given to doctest
                -> Counts   -- ^ expected test result
                -> Test
doctestTestCase doctest dir args counts = TestCase $ doctestAssert doctest dir args counts


-- | Construct a doctest specific 'Assertion'.
doctestAssert :: FilePath   -- ^ absolute path to `doctest` binary
              -> FilePath   -- ^ current directory of forked `doctest` process
              -> [String]   -- ^ args, given to `doctest`
              -> Counts     -- ^ expected test result
              -> Assertion
doctestAssert doctest workingDir args counts = do
  out <- runDoctest doctest workingDir args
  assertEqual label (showCounts counts) (last . lines $ out)
  where
    label = workingDir ++ " " ++ show args


-- | Fork and run a `doctest` process.
runDoctest :: FilePath      -- ^ absolute path to `doctest` binary
           -> FilePath      -- ^ current directory of forked `doctest` process
           -> [String]      -- ^ args, given to `doctest`
           -> IO String     -- ^ final result, as printed by `doctest`
runDoctest doctest workingDir args = do
  cwd <- getCurrentDirectory
  setCurrentDirectory workingDir
  (exit, out, err) <- readProcessWithExitCode doctest args ""
  setCurrentDirectory cwd
  return $ lastLine err
  where
    lastLine = reverse . takeWhile (/= '\r') . reverse
