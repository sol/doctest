module RunSpec (main, spec) where

import           Test.Hspec.ShouldBe hiding (Summary)
import           System.Exit

import           Control.Exception
import           System.Directory (getCurrentDirectory, setCurrentDirectory)

import           System.IO.Silently
import           System.IO (stderr)

import           Run

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory workingDir action = do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory workingDir
    action

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "doctest" $ do
    it "exits with ExitFailure, if at least one test case fails" $ do
      hSilence [stderr] (doctest ["test/integration/failing/Foo.hs"]) `shouldThrow` (== ExitFailure 1)

  describe "doctest_" $ do
    context "on parse error" $ do
      let action = withCurrentDirectory "test/integration/parse-error" (doctest_ ["Foo.hs"])

      it "aborts with (ExitFailure 1)" $ do
        hSilence [stderr] action `shouldThrow` (== ExitFailure 1)

      it "prints a useful error message" $ do
        (r, _) <- hCapture [stderr] (try action :: IO (Either ExitCode Summary))
        r `shouldBe` "\nFoo.hs:6:1: parse error (possibly incorrect indentation)\n"
