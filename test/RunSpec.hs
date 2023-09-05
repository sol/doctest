{-# LANGUAGE CPP #-}
module RunSpec (main, spec) where

import           Imports

import           Test.Hspec
import           System.Exit

import qualified Control.Exception as E
import           System.FilePath
import           System.Directory (getCurrentDirectory, setCurrentDirectory)
import           Data.List (isPrefixOf, sort)
import           Data.Char

import           System.IO.Silently
import           System.IO (stderr)
import qualified Options

import           Run

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory workingDir action = do
  E.bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory workingDir
    action

main :: IO ()
main = hspec spec


removeLoadedPackageEnvironment :: String -> String
#if __GLASGOW_HASKELL__ < 810
removeLoadedPackageEnvironment = unlines . filter (not . isPrefixOf "Loaded package environment from ") . lines
#else
removeLoadedPackageEnvironment = id
#endif

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
      removeLoadedPackageEnvironment r `shouldBe` unlines [
          "doctest: unrecognized option `--foo'"
        , "Try `doctest --help' for more information."
        ]

    it "prints verbose description of a specification" $ do
      (r, ()) <- hCapture [stderr] $ doctest ["--verbose", "test/integration/testSimple/Fib.hs"]
      removeLoadedPackageEnvironment r `shouldBe` unlines [
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
      removeLoadedPackageEnvironment r `shouldBe` unlines [
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
      removeLoadedPackageEnvironment r `shouldBe` unlines [
              "### Started execution at test/integration/failing/Foo.hs:5."
            , "### example:"
            , "23"
            , "test/integration/failing/Foo.hs:5: failure in expression `23'"
            , "expected: 42"
            , " but got: 23"
            , "          ^"
            , ""
            , "# Final summary:"
            , "Examples: 1  Tried: 1  Errors: 0  Failures: 1"
        ]

#if __GLASGOW_HASKELL__ >= 802
    it "can deal with potentially problematic GHC options" $ do
      hSilence [stderr] $ doctest ["-fdiagnostics-color=always", "test/integration/color/Foo.hs"]
#endif

  describe "doctestWithResult" $ do
    context "on parse error" $ do
      let
        action = withCurrentDirectory "test/integration/parse-error" $ do
          doctestWithResult defaultConfig { ghcOptions = ["Foo.hs"] }

      it "aborts with (ExitFailure 1)" $ do
        hSilence [stderr] action `shouldThrow` (== ExitFailure 1)

      it "prints a useful error message" $ do
        (r, _) <- hCapture [stderr] (E.try action :: IO (Either ExitCode Summary))
        stripAnsiColors (removeLoadedPackageEnvironment r) `shouldBe` unlines [
            ""
#if __GLASGOW_HASKELL__ >= 906
          , "Foo.hs:6:1: error: [GHC-58481]"
#else
          , "Foo.hs:6:1: error:"
#endif
          , "    parse error (possibly incorrect indentation or mismatched brackets)"
          ]

  describe "expandDirs" $ do
    it "expands a directory" $ do
      res <- expandDirs "example"
      sort res `shouldBe`
        [ "example" </> "src" </> "Example.hs"
        , "example" </> "test" </> "doctests.hs"
        ]
    it "ignores files" $ do
      res <- expandDirs "doctest.cabal"
      res `shouldBe` ["doctest.cabal"]
    it "ignores random things" $ do
      let x = "foo bar baz bin"
      res <- expandDirs x
      res `shouldBe` [x]

stripAnsiColors :: String -> String
stripAnsiColors xs = case xs of
  '\ESC' : '[' : ';' : ys | 'm' : zs <- dropWhile isNumber ys -> stripAnsiColors zs
  '\ESC' : '[' : ys | 'm' : zs <- dropWhile isNumber ys -> stripAnsiColors zs
  y : ys -> y : stripAnsiColors ys
  [] -> []
