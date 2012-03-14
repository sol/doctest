module ExtractSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit

import           Extract
import           System.FilePath

shouldBeM :: (Show a, Eq a) => IO a -> a -> Assertion
action `shouldBeM` expected = do
  actual <- action
  actual `shouldBe` expected


shouldGive :: (String, String) -> [Module] -> Assertion
(d, m) `shouldGive` expected =
  extract ["-i" ++ dir] [dir </> m] `shouldBeM` expected
  where dir = "extract" </> d

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "extract" $ do
    it "extracts documentation for a top-level declaration" $ do
      ("declaration", "Foo.hs") `shouldGive` [Module "Foo" [" Some documentation"]]

    it "extracts documentation from argument list" $ do
      ("argument-list", "Foo.hs") `shouldGive` [Module "Foo" [" doc for arg1", " doc for arg2"]]

    it "extracts documentation for a type class function" $ do
      ("type-class", "Foo.hs") `shouldGive` [Module "Foo" [" Convert given value to a string."]]

    it "extracts documentation from the argument list of a type class function" $ do
      ("type-class-args", "Foo.hs") `shouldGive` [Module "Foo" [" foo", " bar"]]

    it "extracts documentation from the module header" $ do
      ("module-header", "Foo.hs") `shouldGive` [Module "Foo" [" Some documentation"]]

    it "extracts documentation from imported modules" $ do
      ("imported-module", "Bar.hs") `shouldGive` [Module "Bar" [" documentation for bar"], Module "Baz" [" documentation for baz"]]

    it "fails on invalid flags" $ do
      extract ["--foobar"] ["test/Foo.hs"] `shouldThrow` errorCall "Unrecognized GHC option: --foobar"
