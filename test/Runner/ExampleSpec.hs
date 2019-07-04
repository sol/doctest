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

lineToExpected :: ([ExpectedLine] -> ExpectedResult) -> [Line] -> ExpectedResult
lineToExpected ctor = ctor .
    (map $ \x -> case x of
                  PlainLine str -> fromString str
                  WildCardLines _ -> WildCardLine)

lineToActual :: [Line] -> [String]
lineToActual = concatMap $ \x -> case x of
                               PlainLine str -> [str]
                               WildCardLines xs -> xs

mkRes :: ([ExpectedLine] -> ExpectedResult) -> [ExpectedLine] -> [String] -> Runner.Example.Result
mkRes ctor a b = mkResult (ctor a) b

isNotEqual :: Runner.Example.Result -> Bool
isNotEqual (NotEqual _) = True
isNotEqual _ = False


spec :: Spec
spec = do
  describe "mkRes ExpectedResult" $ do
    it "returns Equal when output matches" $ do
      property $ \xs -> do
        mkRes ExpectedResult (map fromString xs) xs `shouldBe` Equal

    it "ignores trailing whitespace" $ do
      mkRes ExpectedResult ["foo\t"] ["foo  "] `shouldBe` Equal

    context "with WildCardLine" $ do
      it "matches zero lines" $ do
        mkRes ExpectedResult ["foo", WildCardLine, "bar"] ["foo", "bar"]
            `shouldBe` Equal

      it "matches an arbitrary number of lines" $ do
        mkRes ExpectedResult ["foo", WildCardLine, "bar"] ["foo", "baz", "bazoom", "bar"]
            `shouldBe` Equal

      it "matches an arbitrary number of lines (quickcheck)" $ do
        property $ \xs -> mkResult (lineToExpected ExpectedResult xs) (lineToActual xs)
            `shouldBe` Equal

    context "with WildCardChunk" $ do
      it "matches an arbitrary line chunk" $ do
        mkRes ExpectedResult [ExpectedLine ["foo", WildCardChunk, "bar"]] ["foo baz bar"]
            `shouldBe` Equal

    context "when output does not match" $ do
      it "constructs failure message" $ do
        mkRes ExpectedResult ["foo"] ["bar"] `shouldBe` NotEqual [
            "expected: foo"
          , " but got: bar"
          ]

      it "constructs failure message for multi-line output" $ do
        mkRes ExpectedResult ["foo", "bar"] ["foo", "baz"] `shouldBe` NotEqual [
            "expected: foo"
          , "          bar"
          , " but got: foo"
          , "          baz"
          ]

      context "when any output line contains \"unsafe\" characters" $ do
        it "uses show to format output lines" $ do
          mkRes ExpectedResult ["foo\160bar"] ["foo bar"] `shouldBe` NotEqual [
              "expected: \"foo\\160bar\""
            , " but got: \"foo bar\""
            ]

  describe "mkRes UnexpectedResult" $ do
    it "returns Equal when output matches" $ do
      property $ \xs -> do
        mkRes UnexpectedResult (map fromString xs) xs `shouldSatisfy` isNotEqual

    it "ignores trailing whitespace" $ do
      mkRes UnexpectedResult ["foo\t"] ["foo  "]
          `shouldBe` NotEqual ["didn't expect: \"foo\\t\""
                              ," but got: \"foo  \""
                              ]

    context "with WildCardLine" $ do
      it "matches zero lines" $ do
        mkRes UnexpectedResult ["foo", WildCardLine, "bar"] ["foo", "bar"]
            `shouldBe` NotEqual ["didn't expect: foo"
                                ,"               ..."
                                ,"               bar"
                                ," but got: foo"
                                ,"          bar"
                                ]

      it "matches an arbitrary number of lines" $ do
        mkRes UnexpectedResult ["foo", WildCardLine, "bar"] ["foo", "baz", "bazoom", "bar"]
            `shouldBe` NotEqual ["didn't expect: foo"
                                ,"               ..."
                                ,"               bar"
                                ," but got: foo"
                                ,"          baz"
                                ,"          bazoom"
                                ,"          bar"
                                ]


      it "matches an arbitrary number of lines (quickcheck)" $ do
        property $ \xs -> mkResult (lineToExpected UnexpectedResult xs) (lineToActual xs)
            `shouldSatisfy` isNotEqual

    context "with WildCardChunk" $ do
      it "matches an arbitrary line chunk" $ do
        mkRes UnexpectedResult [ExpectedLine ["foo", WildCardChunk, "bar"]] ["foo baz bar"]
            `shouldBe` NotEqual [ "didn't expect: foo...bar"
                                , " but got: foo baz bar"
                                ]

    context "when output does not match" $ do
      it "constructs failure message" $ do
        mkRes UnexpectedResult ["foo"] ["bar"] `shouldBe` Equal

      it "constructs failure message for multi-line output" $ do
        mkRes UnexpectedResult ["foo", "bar"] ["foo", "baz"] `shouldBe` Equal

      context "when any output line contains \"unsafe\" characters" $ do
        it "uses show to format output lines" $ do
          mkRes UnexpectedResult ["foo\160bar"] ["foo bar"] `shouldBe` Equal
