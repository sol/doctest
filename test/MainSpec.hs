module MainSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit (assertEqual, Counts(Counts, tried, errors, failures), Assertion, showCounts)
import qualified Test.HUnit as HU

import           System.Directory (canonicalizePath, getCurrentDirectory, setCurrentDirectory)
import           System.FilePath
import           System.Process (readProcessWithExitCode)


-- | Run doctest and return stderr.
doctest_ :: FilePath   -- ^ current directory of forked `doctest` process
         -> [String]   -- ^ args, given to `doctest`
         -> IO String     -- ^ stderr
doctest_ workingDir args = do
  bin <- canonicalizePath "dist/build/doctest/doctest"

  -- fork and run a doctest process
  cwd <- getCurrentDirectory
  setCurrentDirectory ("test/integration" </> workingDir)
  (_, _, err) <- readProcessWithExitCode bin args ""
  setCurrentDirectory cwd
  return err


-- | Construct a doctest specific 'Assertion'.
doctest :: FilePath   -- ^ current directory of forked `doctest` process
        -> [String]   -- ^ args, given to `doctest`
        -> Counts     -- ^ expected test result
        -> Assertion
doctest workingDir args counts = do
  err <- doctest_ workingDir args
  let out = lastLine err
  assertEqual label (showCounts counts) (last . lines $ out)
  where
    label = workingDir ++ " " ++ show args
    lastLine = reverse . takeWhile (/= '\r') . reverse


cases :: Int -> Counts
cases n = Counts {
  HU.cases = n
, tried    = n
, errors   = 0
, failures = 0
}

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "doctest" $ do
    it "testSimple" $ do
      doctest "." ["testSimple/Fib.hs"]
        (cases 1)

    it "testFail" $ do
      doctest "." ["testFail/Foo.hs"]
        (cases 1) {failures = 1}

    it "testImport" $ do
      doctest "testImport" ["ModuleA.hs"]
        (cases 2)
      doctest ".." ["--optghc=-iintegration/testImport", "integration/testImport/ModuleA.hs"]
        (cases 2)

    it "testCommentLocation" $ do
      doctest "." ["testCommentLocation/Foo.hs"]
        (cases 10)

    it "testPutStr" $ do
      doctest "testPutStr" ["Fib.hs"]
        (cases 1)

    it "testFailOnMultiline" $ do
      doctest "testFailOnMultiline" ["Fib.hs"]
        (cases 1) {errors = 1}

    it "testBlankline" $ do
      doctest "testBlankline" ["Fib.hs"]
        (cases 1)

    it "testCombinedExample" $ do
      doctest "testCombinedExample" ["Fib.hs"]
        (cases 1)

    it "testDocumentationForArguments" $ do
      doctest "testDocumentationForArguments" ["Fib.hs"]
        (cases 1)

    it "template-haskell" $ do
      doctest "template-haskell" ["Foo.hs"]
        (cases 1)

  describe "doctest (regression tests)" $ do
    it "bugfixWorkingDirectory" $ do
      doctest "bugfixWorkingDirectory" ["Fib.hs"]
        (cases 1)
      doctest "bugfixWorkingDirectory" ["examples/Fib.hs"]
        (cases 2)

    it "bugfixOutputToStdErr" $ do
      doctest "bugfixOutputToStdErr" ["Fib.hs"]
        (cases 1)

    it "bugfixMultipleStatements" $ do
      doctest "bugfixMultipleStatements" ["Fib.hs"]
        (cases 1)

    it "bugfixImportHierarchical" $ do
      doctest "bugfixImportHierarchical" ["ModuleA.hs", "ModuleB.hs"]
        (cases 2)

    it "bugfixMultipleModules" $ do
      doctest "bugfixMultipleModules" ["ModuleA.hs"]
        (cases 3)

    it "testCPP" $ do
      doctest "testCPP" ["--optghc=-cpp", "Foo.hs"]
        (cases 1) {failures = 1}
      doctest "testCPP" ["--optghc=-cpp", "--optghc=-DFOO", "Foo.hs"]
        (cases 1)

    it "prints a useful error message on parse errors" $ do
      err <- doctest_ "parse-error" ["Foo.hs"]
      err `shouldBe` "\nFoo.hs:6:1: parse error (possibly incorrect indentation)\n"

    it "handle source files with CRLF line endings" $ do
      doctest "dos-line-endings" ["Fib.hs"]
        (cases 1)
