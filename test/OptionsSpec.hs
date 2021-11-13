module OptionsSpec (spec) where

import           Prelude ()
import           Prelude.Compat
import           Data.List.Compat

import           Test.Hspec
import           Test.QuickCheck

import           Options

newtype NonInteractive = NonInteractive String
  deriving (Eq, Show)

instance Arbitrary NonInteractive where
  arbitrary = NonInteractive <$> elements (nonInteractiveGhcOptions \\ ["--info"])

spec :: Spec
spec = do
  describe "parseOptions" $ do
    let warning = ["WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."]
    it "strips --optghc" $
      property $ \xs ys ->
        parseOptions (xs ++ ["--optghc", "foobar"] ++ ys) `shouldBe` Result (Run warning (xs ++ ["foobar"] ++ ys) defaultMagic defaultFastMode defaultPreserveIt defaultVerbose)

    it "strips --optghc=" $
      property $ \xs ys ->
        parseOptions (xs ++ ["--optghc=foobar"] ++ ys) `shouldBe` Result (Run warning (xs ++ ["foobar"] ++ ys) defaultMagic defaultFastMode defaultPreserveIt defaultVerbose)

    context "with ghc options that are not valid with --interactive" $ do
      it "returns RunGhc" $ do
        property $ \ (NonInteractive x) xs -> do
          let options = x : xs
          parseOptions options `shouldBe` RunGhc options

    context "with --interactive" $ do
      let options = ["--interactive", "--foo", "--bar"]

      it "disables magic mode" $ do
        runMagicMode <$> parseOptions options `shouldBe` Result False

      it "filters out --interactive" $ do
        runOptions <$> parseOptions options `shouldBe` Result ["--foo", "--bar"]

    describe "--no-magic" $ do
      context "without --no-magic" $ do
        it "enables magic mode" $ do
          runMagicMode <$> parseOptions [] `shouldBe` Result True

      context "with --no-magic" $ do
        it "disables magic mode" $ do
          runMagicMode <$> parseOptions ["--no-magic"] `shouldBe` Result False

    describe "--fast" $ do
      context "without --fast" $ do
        it "disables fast mode" $ do
          runFastMode <$> parseOptions [] `shouldBe` Result False

      context "with --fast" $ do
        it "enabled fast mode" $ do
          runFastMode <$> parseOptions ["--fast"] `shouldBe` Result True

    describe "--preserve-it" $ do
      context "without --preserve-it" $ do
        it "does not preserve the `it` variable" $ do
          runPreserveIt <$> parseOptions [] `shouldBe` Result False

      context "with --preserve-it" $ do
        it "preserves the `it` variable" $ do
          runPreserveIt <$> parseOptions ["--preserve-it"] `shouldBe` Result True

    context "with --help" $ do
      it "outputs usage information" $ do
        parseOptions ["--help"] `shouldBe` Output usage

    context "with --version" $ do
      it "outputs version information" $ do
        parseOptions ["--version"] `shouldBe` Output versionInfo

    context "with --info" $ do
      it "outputs machine readable version information" $ do
        parseOptions ["--info"] `shouldBe` Output info

    describe "--verbose" $ do
      context "without --verbose" $ do
        it "is not verbose by default" $ do
          runVerbose <$> parseOptions [] `shouldBe` Result False

      context "with --verbose" $ do
        it "parses verbose option" $ do
          runVerbose <$> parseOptions ["--verbose"] `shouldBe` Result True
