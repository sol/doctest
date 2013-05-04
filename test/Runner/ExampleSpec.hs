module Runner.ExampleSpec (main, spec) where

import           Test.Hspec

import           Runner.Example

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mkResult" $ do
    it "works for one-line test output" $ do
      mkResult ["foo"] ["bar"] `shouldBe` NotEqual [
          "expected: foo"
        , " but got: bar"
        ]

    it "works for multi-line test output" $ do
      mkResult ["foo", "bar"] ["foo", "baz"] `shouldBe` NotEqual [
        "expected: foo"
        , "          bar"
        , " but got: foo"
        , "          baz"
        ]

    it "quotes output if any output line ends with trailing whitespace" $ do
      mkResult ["foo", "bar   "] ["foo", "baz"] `shouldBe` NotEqual [
        "expected: \"foo\""
        , "          \"bar   \""
        , " but got: \"foo\""
        , "          \"baz\""
        ]

    it "uses show to format output lines if any output line contains \"unsafe\" characters" $ do
      mkResult ["foo\160bar"] ["foo bar"] `shouldBe` NotEqual [
          "expected: \"foo\\160bar\""
        , " but got: \"foo bar\""
        ]
