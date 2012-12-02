{-# LANGUAGE CPP #-}
module RunSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           System.Exit

import           Control.Exception
import           System.Cmd
import           System.Directory (getCurrentDirectory, setCurrentDirectory, removeDirectoryRecursive)
import           Data.List

import qualified Control.Exception as E
import           System.Environment
import           System.SetEnv

import           System.IO.Silently
import           System.IO (stderr)
import qualified Help

import           Run

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory workingDir action = do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory workingDir
    action

withEnv :: String -> String -> IO a -> IO a
withEnv k v action = E.bracket save restore $ \_ -> do
  setEnv k v >> action
  where
    save    = lookupEnv k
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
      r `shouldBe` Help.usage

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
      (r, e) <- hCapture [stderr] . try $ doctest ["--foo", "test/integration/test-options/Foo.hs"]
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

      removeDirectoryRecursive "test/integration/custom-package-conf/packages/"
      removeDirectoryRecursive "test/integration/custom-package-conf/foo/dist/"

  describe "doctest_" $ do
    context "on parse error" $ do
      let action = withCurrentDirectory "test/integration/parse-error" (doctest_ ["Foo.hs"])

      it "aborts with (ExitFailure 1)" $ do
        hSilence [stderr] action `shouldThrow` (== ExitFailure 1)

      it "prints a useful error message" $ do
        (r, _) <- hCapture [stderr] (try action :: IO (Either ExitCode Summary))
#if __GLASGOW_HASKELL__ < 706
        r `shouldBe` "\nFoo.hs:6:1: parse error (possibly incorrect indentation)\n"
#else
        r `shouldBe` "\nFoo.hs:6:1:\n    parse error (possibly incorrect indentation or mismatched brackets)\n"
#endif

  describe "stripOptGhc (an internal function)" $ do
    it "strips --optghc=" $
      property $ \xs ys ->
        stripOptGhc (xs ++ ["--optghc=foobar"] ++ ys) == (True, xs ++ ["foobar"] ++ ys)

    it "strips --optghc" $
      property $ \xs ys ->
        stripOptGhc (xs ++ ["--optghc", "foobar"] ++ ys) == (True, xs ++ ["foobar"] ++ ys)

    it "indicates when nothing got striped" $
      property $ \xs ->
        stripOptGhc xs == (False, xs)
