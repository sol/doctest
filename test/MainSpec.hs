{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module MainSpec (main, spec) where

import           Imports

import           Test.Hspec
import           Test.HUnit (assertEqual, Assertion)

import           System.Directory (getCurrentDirectory, setCurrentDirectory)
import           System.FilePath
import           Run hiding (doctest)
import           System.IO.Silently
import           System.IO

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory workingDir action = do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory workingDir
    action

-- | Construct a doctest specific 'Assertion'.
doctest :: HasCallStack => FilePath -> [String] -> Summary -> Assertion
doctest = doctestWithPreserveIt False

doctestWithPreserveIt :: HasCallStack => Bool -> FilePath -> [String] -> Summary -> Assertion
doctestWithPreserveIt preserveIt workingDir ghcOptions expected = do
  actual <- withCurrentDirectory ("test/integration" </> workingDir) (hSilence [stderr] $ doctestWithResult defaultConfig {ghcOptions, preserveIt})
  assertEqual label (formatSummary expected) (formatSummary actual)
  where
    label = workingDir ++ " " ++ show ghcOptions

cases :: Int -> Summary
cases n = Summary n n 0 0

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "doctest" $ do
    it "testSimple" $ do
      doctest "." ["testSimple/Fib.hs"]
        (cases 1)

    it "it-variable" $ do
      doctestWithPreserveIt True "." ["it/Foo.hs"]
        (cases 5)

    it "it-variable in $setup" $ do
      doctestWithPreserveIt True "." ["it/Setup.hs"]
        (cases 5)

    it "failing" $ do
      doctest "." ["failing/Foo.hs"]
        (cases 1) {sFailures = 1}

    it "skips subsequent examples from the same group if an example fails" $
      doctest "." ["failing-multiple/Foo.hs"]
        (cases 4) {sTried = 2, sFailures = 1}

    it "testImport" $ do
      doctest "testImport" ["ModuleA.hs"]
        (cases 3)
      doctest ".." ["-iintegration/testImport", "integration/testImport/ModuleA.hs"]
        (cases 3)

    it "testCommentLocation" $ do
      doctest "." ["testCommentLocation/Foo.hs"]
        (cases 11)

    it "testPutStr" $ do
      doctest "testPutStr" ["Fib.hs"]
        (cases 3)

    it "fails on multi-line expressions, introduced with :{" $ do
      doctest "testFailOnMultiline" ["Fib.hs"]
        (cases 2) {sErrors = 2}

    it "testBlankline" $ do
      doctest "testBlankline" ["Fib.hs"]
        (cases 1)

    it "examples from the same Haddock comment share the same scope" $ do
      doctest "testCombinedExample" ["Fib.hs"]
        (cases 4)

    it "testDocumentationForArguments" $ do
      doctest "testDocumentationForArguments" ["Fib.hs"]
        (cases 1)

    it "template-haskell" $ do
      doctest "template-haskell" ["Foo.hs"]
        (cases 2)

    it "handles source files with CRLF line endings" $ do
      doctest "dos-line-endings" ["Fib.hs"]
        (cases 1)

    it "runs $setup before each test group" $ do
      doctest "setup" ["Foo.hs"]
        (cases 2)

    it "skips subsequent tests from a module, if $setup fails" $ do
      doctest "setup-skip-on-failure" ["Foo.hs"]
        (cases 3) {sTried = 1, sFailures = 1}

    -- Andreas, 2021-02-27, see issue #294.
    -- This test case contains a hard-wired path that does not work
    -- with v2-cabal.
    -- I tested it under v2-cabal with a very non-portable path.
    -- Deactivating the test case until a systematic solution is found...
    --
    -- it "works with additional object files" $ do
    --   -- -- Path for v1-cabal:
    --   -- doctest "with-cbits" ["Bar.hs", "../../../dist/build/spec/spec-tmp/test/integration/with-cbits/foo.o"]
    --   -- -- Path for v2-cabal with ghc-9.0.1
    --   -- doctest "with-cbits" ["Bar.hs", "../../../dist-newstyle/build/x86_64-osx/ghc-9.0.1/doctest-0.19/t/spec/build/spec/spec-tmp/Test/integration/with-cbits/foo.o"]
    --     (cases 1)

    it "ignores trailing whitespace when matching test output" $ do
      doctest "trailing-whitespace" ["Foo.hs"]
        (cases 1)

  describe "doctest as a runner for QuickCheck properties" $ do
    it "runs a boolean property" $ do
      doctest "property-bool" ["Foo.hs"]
        (cases 1)

    it "runs an explicitly quantified property" $ do
      doctest "property-quantified" ["Foo.hs"]
        (cases 1)

    it "runs an implicitly quantified property" $ do
      doctest "property-implicitly-quantified" ["Foo.hs"]
        (cases 1)

    it "reports a failing property" $ do
      doctest "property-failing" ["Foo.hs"]
        (cases 1) {sFailures = 1}

    it "runs a boolean property with an explicit type signature" $ do
      doctest "property-bool-with-type-signature" ["Foo.hs"]
        (cases 1)

    it "runs $setup before each property" $ do
      doctest "property-setup" ["Foo.hs"]
        (cases 3)

  describe "doctest (regression tests)" $ do
    it "bugfixWorkingDirectory" $ do
      doctest "bugfixWorkingDirectory" ["Fib.hs"]
        (cases 1)
      doctest "bugfixWorkingDirectory" ["examples/Fib.hs"]
        (cases 2)

    it "bugfixOutputToStdErr" $ do
      doctest "bugfixOutputToStdErr" ["Fib.hs"]
        (cases 2)

    it "bugfixImportHierarchical" $ do
      doctest "bugfixImportHierarchical" ["ModuleA.hs", "ModuleB.hs"]
        (cases 3)

    it "bugfixMultipleModules" $ do
      doctest "bugfixMultipleModules" ["ModuleA.hs"]
        (cases 5)

    it "testCPP" $ do
      doctest "testCPP" ["-cpp", "Foo.hs"]
        (cases 1) {sFailures = 1}
      doctest "testCPP" ["-cpp", "-DFOO", "Foo.hs"]
        (cases 1)

    it "template-haskell-bugfix" $ do
      doctest "template-haskell-bugfix" ["Main.hs"]
        (cases 2)

    it "doesn't clash with user bindings of stdout/stderr" $ do
      doctest "local-stderr-binding" ["A.hs"]
        (cases 1)

    it "doesn't get confused by doctests using System.IO imports" $ do
      doctest "system-io-imported" ["A.hs"]
        (cases 1)
