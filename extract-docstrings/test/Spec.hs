module Spec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit

import           Extract (extract)

shouldBeM :: (Show a, Eq a) => IO a -> a -> Assertion
action `shouldBeM` expected = do
  actual <- action
  actual `shouldBe` expected


(dir, m) `shouldGive` expected = do
  extract ["-i" ++ dir] [dir ++ "/" ++ m] `shouldBeM` expected

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "extract" $ do
    it "extracts documentation for a top-level declaration" $ do
      ("declaration", "Foo.hs") `shouldGive` [" Some documentation"]

    it "extracts documentation from the module header" $ do
      ("module-header", "Foo.hs") `shouldGive` [" Some documentation"]

    it "extracts documentation from imported modules" $ do
      ("imported-module", "Bar.hs") `shouldGive` [" documentation for bar", " documentation for baz"]

    it "fails on invalid flags" $ do
      extract ["--foobar"] ["test/Foo.hs"] `shouldThrow` errorCall "Unrecognized GHC option: --foobar"
