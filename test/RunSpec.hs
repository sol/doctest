{-# LANGUAGE CPP #-}
module RunSpec (main, spec) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec
import           System.Exit

import qualified Control.Exception as E
#if __GLASGOW_HASKELL__ < 707
import           System.Cmd
#else
import           System.Process
#endif
import           System.Directory (getCurrentDirectory, setCurrentDirectory, removeDirectoryRecursive)
import           Data.List.Compat

import           System.Environment.Compat

import           System.IO.Silently
import           System.IO (stderr)
import qualified Options

import           Run

doctestWithDefaultOptions :: [String] -> IO Summary
doctestWithDefaultOptions = doctestWithOptions Options.defaultFastMode Options.defaultPreserveIt Options.defaultInterpret Options.defaultVerbose

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory workingDir action = do
  E.bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory workingDir
    action

rmDir :: FilePath -> IO ()
rmDir dir = removeDirectoryRecursive dir `E.catch` (const $ return () :: E.IOException -> IO ())

withEnv :: String -> String -> IO a -> IO a
withEnv k v action = E.bracket save restore $ \_ -> do
  setEnv k v >> action
  where
    save    = lookup k <$> getEnvironment
    restore = maybe (unsetEnv k) (setEnv k)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "doctest" $ do
    it "exits with ExitFailure if at least one test case fails" $ do
      hSilence [stderr] (doctest ["test/integration/failing/Foo.hs"]) `shouldThrow` (== ExitFailure 1)

    it "prints help on --help" $ do
      (r, ()) <- capture (doctest ["--help"])
      r `shouldBe` Options.usage

    it "prints version on --version" $ do
      (r, ()) <- capture (doctest ["--version"])
      lines r `shouldSatisfy` any (isPrefixOf "doctest version ")

    it "accepts arbitrary GHC options" $ do
      hSilence [stderr] $ doctest ["-cpp", "-DFOO", "test/integration/test-options/Foo.hs"]

    it "accepts GHC options with --optghc" $ do
      hSilence [stderr] $ doctest ["--optghc=-cpp", "--optghc=-DFOO", "test/integration/test-options/Foo.hs"]

    it "prints a deprecation message for --optghc" $ do
      (r, _) <- hCapture [stderr] $ doctest ["--optghc=-cpp", "--optghc=-DFOO", "test/integration/test-options/Foo.hs"]
      lines r `shouldSatisfy` isPrefixOf [
          "WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options"
        , "directly."
        ]

    it "prints error message on invalid option" $ do
      (r, e) <- hCapture [stderr] . E.try $ doctest ["--foo", "test/integration/test-options/Foo.hs"]
      e `shouldBe` Left (ExitFailure 1)
      r `shouldBe` unlines [
          "doctest: unrecognized option `--foo'"
        , "Try `doctest --help' for more information."
        ]

    it "respects HASKELL_PACKAGE_SANDBOX" $ do
      withCurrentDirectory "test/integration/custom-package-conf/foo" $ do
        ExitSuccess <- rawSystem "ghc-pkg" ["init", "../packages"]
        ExitSuccess <- rawSystem "cabal" ["configure", "--disable-optimization", "--disable-library-profiling", "--package-db=../packages"]
        ExitSuccess <- rawSystem "cabal" ["build"]
        ExitSuccess <- rawSystem "cabal" ["register", "--inplace"]
        return ()

      withEnv "HASKELL_PACKAGE_SANDBOX" "test/integration/custom-package-conf/packages" $ do
        hCapture_ [stderr] (doctest ["test/integration/custom-package-conf/Bar.hs"])
          `shouldReturn` "Examples: 2  Tried: 2  Errors: 0  Failures: 0\n"

      `E.finally` do
        rmDir "test/integration/custom-package-conf/packages/"
        rmDir "test/integration/custom-package-conf/foo/dist/"

    it "prints verbose description of a specification" $ do
      (r, ()) <- hCapture [stderr] $ doctest ["--verbose", "test/integration/testSimple/Fib.hs"]
      r `shouldBe` unlines [
          "### Started execution at test/integration/testSimple/Fib.hs:5."
        , "### example:"
        , "fib 10"
        , "### Successful!"
        , ""
        , "# Final summary:"
        , "Examples: 1  Tried: 1  Errors: 0  Failures: 0"
        ]

    it "prints verbose description of a property" $ do
      (r, ()) <- hCapture [stderr] $ doctest ["--verbose", "test/integration/property-bool/Foo.hs"]
      r `shouldBe` unlines [
          "### Started execution at test/integration/property-bool/Foo.hs:4."
        , "### property:"
        , "True"
        , "### Successful!"
        , ""
        , "# Final summary:"
        , "Examples: 1  Tried: 1  Errors: 0  Failures: 0"
        ]

    it "prints verbose error" $ do
      (r, e) <- hCapture [stderr] . E.try $ doctest ["--verbose", "test/integration/failing/Foo.hs"]
      e `shouldBe` Left (ExitFailure 1)
      r `shouldBe` unlines [
              "### Started execution at test/integration/failing/Foo.hs:5."
            , "### example:"
            , "23"
            , "test/integration/failing/Foo.hs:5: failure in expression `23'"
            , "expected: 42"
            , " but got: 23"
            , ""
            , "# Final summary:"
            , "Examples: 1  Tried: 1  Errors: 0  Failures: 1"
        ]

  describe "doctestWithOptions" $ do
    context "on parse error" $ do
      let action = withCurrentDirectory "test/integration/parse-error" (doctestWithDefaultOptions ["Foo.hs"])

      it "aborts with (ExitFailure 1)" $ do
        hSilence [stderr] action `shouldThrow` (== ExitFailure 1)

      it "prints a useful error message" $ do
        (r, _) <- hCapture [stderr] (E.try action :: IO (Either ExitCode Summary))
#if __GLASGOW_HASKELL__ < 706
        r `shouldBe` "\nFoo.hs:6:1: parse error (possibly incorrect indentation)\n"
#else

#if __GLASGOW_HASKELL__ < 800
        r `shouldBe` "\nFoo.hs:6:1:\n    parse error (possibly incorrect indentation or mismatched brackets)\n"
#else
        r `shouldBe` "\nFoo.hs:6:1: error:\n    parse error (possibly incorrect indentation or mismatched brackets)\n"
#endif

#endif

  describe "expandDirs" $ do
    it "expands a directory" $ do
      res <- expandDirs "example"
      sort res `shouldBe`
        [ "example/src/Example.hs"
        , "example/test/doctests.hs"
        ]
    it "ignores files" $ do
      res <- expandDirs "doctest.cabal"
      res `shouldBe` ["doctest.cabal"]
    it "ignores random things" $ do
      let x = "foo bar baz bin"
      res <- expandDirs x
      res `shouldBe` [x]
