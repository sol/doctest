module Runner.ExampleSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Runner.Example

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mkResult" $ do
    it "returns Equal when output matches" $ do
      property $ \xs -> do
        mkResult xs xs `shouldBe` Equal

    it "ignores trailing whitespace" $ do
      mkResult ["foo\t"] ["foo  "] `shouldBe` Equal

    context "when output does not matche" $ do
      it "constructs failure message" $ do
        mkResult ["foo"] ["bar"] `shouldBe` NotEqual [
            "expected: foo"
          , " but got: bar"
          ]

      it "constructs failure message for multi-line output" $ do
        mkResult ["foo", "bar"] ["foo", "baz"] `shouldBe` NotEqual [
            "expected: foo"
          , "          bar"
          , " but got: foo"
          , "          baz"
          ]

      context "when any output line contains \"unsafe\" characters" $ do
        it "uses show to format output lines" $ do
          mkResult ["foo\160bar"] ["foo bar"] `shouldBe` NotEqual [
              "expected: \"foo\\160bar\""
            , " but got: \"foo bar\""
            ]
