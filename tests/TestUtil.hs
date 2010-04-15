module TestUtil (
    doctestTestCase
  , TestUtil.cases
  , errors
  , failures
  ) where

import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)
import qualified Test.HUnit as HU
import Test.HUnit (assertEqual, Counts(..), Test(TestCase), Assertion,
                   showCounts)
import Data.String.Utils (strip, split)

cases :: Int -> Counts
cases n = Counts {
    HU.cases = n
  , tried    = n
  , errors   = 0
  , failures = 0
  }


-- | Construct a doctest specific 'TestCase'.
doctestTestCase :: FilePath -- | absolute path to `doctest` binary
                -> FilePath -- | current directory of forked `doctest` process
                -> [String] -- | args, given to doctest
                -> Counts   -- | expected test result
                -> Test
doctestTestCase doctest dir args counts = TestCase $ doctestAssert doctest dir args counts


-- | Construct a doctest specific 'Assertion'.
doctestAssert :: FilePath   -- | absolute path to `doctest` binary
              -> FilePath   -- | current directory of forked `doctest` process
              -> [String]   -- | args, given to `doctest`
              -> Counts     -- | expected test result
              -> Assertion
doctestAssert doctest workingDir args counts = do
  out <- runDoctest doctest workingDir args
  assertEqual label (showCounts counts) (last . lines $ out)
  where
    label = workingDir ++ " " ++ show args


-- | For and run a `doctest` process.
runDoctest :: FilePath      -- | absolute path to `doctest` binary
           -> FilePath      -- | current directory of forked `doctest` process
           -> [String]      -- | args, given to `doctest`
           -> IO String     -- | final result, as printed by `doctest`
runDoctest doctest workingDir args = do
  cwd <- getCurrentDirectory
  setCurrentDirectory workingDir
  (exit, out, err) <- readProcessWithExitCode doctest args ""
  setCurrentDirectory cwd
  case exit of
    ExitSuccess -> return $ lastLine err
    _           ->
      error $ mayCat "STDERR:" (strip err) ++ mayCat "STDOUT:" (strip out)
      where
        mayCat x y = if null y then "" else unlines ["", x, y]
  where
    lastLine = strip . last . split "\r"
