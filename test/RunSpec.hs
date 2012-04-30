module RunSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           System.Exit

import           Run

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "doctest" $ do
    it "exits with ExitFailure, if at least one test case fails" $ do
      doctest ["test/integration/failing/Foo.hs"] `shouldThrow` (== ExitFailure 1)
