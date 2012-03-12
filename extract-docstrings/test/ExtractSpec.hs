module ExtractSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit

import           Extract (extract)
import           System.FilePath

shouldBeM :: (Show a, Eq a) => IO a -> a -> Assertion
action `shouldBeM` expected = do
  actual <- action
  actual `shouldBe` expected


shouldGive :: (String, String) -> [String] -> Assertion
(d, m) `shouldGive` expected =
  extract ["-i" ++ dir] [dir </> m] `shouldBeM` expected
  where dir = "extract" </> d

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "extract" $ do
    it "extracts documentation for a top-level declaration" $ do
      ("declaration", "Foo.hs") `shouldGive` [" Some documentation"]

    it "extracts documentation from argument list" $ do
      ("argument-list", "Foo.hs") `shouldGive` [" doc for arg1", " doc for arg2"]

    it "extracts documentation for a type class function" $ do
      ("type-class", "Foo.hs") `shouldGive` [" Convert given value to a string."]

    it "extracts documentation from the argument list of a type class function" $ do
      ("type-class-args", "Foo.hs") `shouldGive` [" foo", " bar"]

    it "extracts documentation from the module header" $ do
      ("module-header", "Foo.hs") `shouldGive` [" Some documentation"]

    it "extracts documentation from imported modules" $ do
      ("imported-module", "Bar.hs") `shouldGive` [" documentation for bar", " documentation for baz"]

    it "fails on invalid flags" $ do
      extract ["--foobar"] ["test/Foo.hs"] `shouldThrow` errorCall "Unrecognized GHC option: --foobar"
