module Spec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit

import           Extract (extract)

shouldBeM :: (Show a, Eq a) => IO a -> a -> Assertion
action `shouldBeM` expected = do
  actual <- action
  actual `shouldBe` expected

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "extract" $ do
    it "extracts documentation for a top-level declaration" $ do
      extract [] ["test/Foo.hs"] `shouldBeM` [" Some documentation"]

    it "extracts documentation from the module header" $ do
      extract [] ["test/ModuleHeader.hs"] `shouldBeM` [" Some documentation"]

    it "extracts documentation from imported modules" $ do
      extract ["-itest"] ["test/Bar.hs"] `shouldBeM` [" documentation for bar", " documentation for baz"]

    it "fails on invalid flags" $ do
      extract ["--foobar"] ["test/Foo.hs"] `shouldThrow` errorCall "Unrecognized GHC option: --foobar"
