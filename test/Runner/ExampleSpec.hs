{-# LANGUAGE OverloadedStrings #-}
module Runner.ExampleSpec (main, spec) where

import           Prelude ()
import           Prelude.Compat

import           Data.String
import           Test.Hspec
import           Test.QuickCheck

import           Parse
import           Runner.Example

main :: IO ()
main = hspec spec

data Line = PlainLine String | WildCardLines [String]
  deriving (Show, Eq)

instance Arbitrary Line where
    arbitrary = frequency [ (2, PlainLine <$> arbitrary)
                          , (1, WildCardLines . getNonEmpty <$> arbitrary)
                          ]

lineToExpected :: [Line] -> ExpectedResult
lineToExpected = ExpectedResult .
    (map $ \x -> case x of
                  PlainLine str -> fromString str
                  WildCardLines _ -> WildCardLine)

lineToActual :: [Line] -> [String]
lineToActual = concatMap $ \x -> case x of
                               PlainLine str -> [str]
                               WildCardLines xs -> xs

mkExpectedResult :: [ExpectedLine] -> [String] -> Runner.Example.Result
mkExpectedResult a b = mkResult (ExpectedResult a) b

mkUnexpectedResult :: [ExpectedLine] -> [String] -> Runner.Example.Result
mkUnexpectedResult a b = mkResult (UnexpectedResult a) b

spec :: Spec
spec = do
  describe "mkExpectedResult" $ do
    it "returns Equal when output matches" $ do
      property $ \xs -> do
        mkExpectedResult (map fromString xs) xs `shouldBe` Equal

    it "ignores trailing whitespace" $ do
      mkExpectedResult ["foo\t"] ["foo  "] `shouldBe` Equal

    context "with WildCardLine" $ do
      it "matches zero lines" $ do
        mkExpectedResult ["foo", WildCardLine, "bar"] ["foo", "bar"]
            `shouldBe` Equal

      it "matches an arbitrary number of lines" $ do
        mkExpectedResult ["foo", WildCardLine, "bar"] ["foo", "baz", "bazoom", "bar"]
            `shouldBe` Equal

      it "matches an arbitrary number of lines (quickcheck)" $ do
        property $ \xs -> mkResult (lineToExpected xs) (lineToActual xs)
            `shouldBe` Equal

    context "with WildCardChunk" $ do
      it "matches an arbitrary line chunk" $ do
        mkExpectedResult [ExpectedLine ["foo", WildCardChunk, "bar"]] ["foo baz bar"]
            `shouldBe` Equal

    context "when output does not match" $ do
      it "constructs failure message" $ do
        mkExpectedResult ["foo"] ["bar"] `shouldBe` NotEqual [
            "expected: foo"
          , " but got: bar"
          ]

      it "constructs failure message for multi-line output" $ do
        mkExpectedResult ["foo", "bar"] ["foo", "baz"] `shouldBe` NotEqual [
            "expected: foo"
          , "          bar"
          , " but got: foo"
          , "          baz"
          ]

      context "when any output line contains \"unsafe\" characters" $ do
        it "uses show to format output lines" $ do
          mkExpectedResult ["foo\160bar"] ["foo bar"] `shouldBe` NotEqual [
              "expected: \"foo\\160bar\""
            , " but got: \"foo bar\""
            ]

  describe "mkUnexpectedResult" $ do
    it "returns Equal when output matches" $ do
      property $ \xs -> do
        mkUnexpectedResult (map fromString xs) xs `shouldBe` NotEqual [""]

    it "ignores trailing whitespace" $ do
      mkUnexpectedResult ["foo\t"] ["foo  "] `shouldBe` NotEqual [""]

    context "with WildCardLine" $ do
      it "matches zero lines" $ do
        mkUnexpectedResult ["foo", WildCardLine, "bar"] ["foo", "bar"]
            `shouldBe` NotEqual [""]

      it "matches an arbitrary number of lines" $ do
        mkUnexpectedResult ["foo", WildCardLine, "bar"] ["foo", "baz", "bazoom", "bar"]
            `shouldBe` NotEqual [""]

    context "with WildCardChunk" $ do
      it "matches an arbitrary line chunk" $ do
        mkUnexpectedResult [ExpectedLine ["foo", WildCardChunk, "bar"]] ["foo baz bar"]
            `shouldBe` NotEqual [""]

    context "when output does not match" $ do
      it "constructs failure message" $ do
        mkUnexpectedResult ["foo"] ["bar"] `shouldBe` Equal

      it "constructs failure message for multi-line output" $ do
        mkUnexpectedResult ["foo", "bar"] ["foo", "baz"] `shouldBe` Equal

      context "when any output line contains \"unsafe\" characters" $ do
        it "uses show to format output lines" $ do
          mkUnexpectedResult ["foo\160bar"] ["foo bar"] `shouldBe` Equal
