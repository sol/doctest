module OptionsSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Options

spec :: Spec
spec = do
  describe "parseOptions" $ do
    let warning = ["WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."]
    it "strips --optghc" $
      property $ \xs ys ->
        parseOptions (xs ++ ["--optghc", "foobar"] ++ ys) `shouldBe` Result (Run warning (xs ++ ["foobar"] ++ ys) True False)

    it "strips --optghc=" $
      property $ \xs ys ->
        parseOptions (xs ++ ["--optghc=foobar"] ++ ys) `shouldBe` Result (Run warning (xs ++ ["foobar"] ++ ys) True False)

    context "with --no-magic" $ do
      it "disables magic mode" $ do
        parseOptions ["--no-magic"] `shouldBe` Result (Run [] [] False False)

    context "with --fast" $ do
      it "enabled fast mode" $ do
        parseOptions ["--fast"] `shouldBe` Result (Run [] [] True True)

    context "with --help" $ do
      it "outputs usage information" $ do
        parseOptions ["--help"] `shouldBe` Output usage

    context "with --version" $ do
      it "outputs version information" $ do
        parseOptions ["--version"] `shouldBe` Output versionInfo

    context "with --info" $ do
      it "outputs machine readable version information" $ do
        parseOptions ["--info"] `shouldBe` Output info
