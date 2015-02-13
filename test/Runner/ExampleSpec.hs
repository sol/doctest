{-# LANGUAGE OverloadedStrings #-}
module Runner.ExampleSpec (main, spec) where

import           Control.Applicative
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
lineToExpected = map $ \x -> case x of
                                 PlainLine str -> PlainResultLine str
                                 WildCardLines _ -> WildCardLine

lineToActual :: [Line] -> [String]
lineToActual = concatMap $ \x -> case x of
                               PlainLine str -> [str]
                               WildCardLines xs -> xs

spec :: Spec
spec = do
  describe "mkResult" $ do
    it "returns Equal when output matches" $ do
      property $ \xs -> do
        mkResult (map PlainResultLine xs) xs `shouldBe` Equal

    it "ignores trailing whitespace" $ do
      mkResult ["foo\t"] ["foo  "] `shouldBe` Equal

    context "with WildCardLine" $ do
      it "matches an arbitrary number of lines" $ do
        mkResult ["foo", WildCardLine, "bar"] ["foo", "baz", "bazoom", "bar"]
            `shouldBe` Equal

      it "matches an arbitrary number of lines (quickcheck)" $ do
        property $ \xs -> mkResult (lineToExpected xs) (lineToActual xs)
            `shouldBe` Equal

    context "when output does not match" $ do
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
