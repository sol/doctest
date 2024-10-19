module OptionsSpec (spec) where

import           Imports

import           Data.List

import           Test.Hspec
import           Test.QuickCheck hiding (verbose)

import           Options

newtype NonInteractive = NonInteractive String
  deriving (Eq, Show)

instance Arbitrary NonInteractive where
  arbitrary = NonInteractive <$> elements (nonInteractiveGhcOptions \\ ["--info"])

spec :: Spec
spec = do
  describe "parseOptions" $ do
    let
      run :: [String] -> Run
      run ghcOptions = defaultRun {
        runWarnings = ["WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."]
      , runMagicMode = True
      , runConfig = defaultConfig { ghcOptions }
      }

    it "strips --optghc" $
      parseOptions ["--optghc", "foobar"] `shouldBe` Result (run ["foobar"])

    it "strips --optghc=" $
      parseOptions ["--optghc=foobar"] `shouldBe` Result (run ["foobar"])

    context "with ghc options that are not valid with --interactive" $ do
      it "returns ProxyToGhc" $ do
        property $ \ (NonInteractive x) xs -> do
          let options = x : xs
          parseOptions options `shouldBe` ProxyToGhc options

    context "with --interactive" $ do
      let options = ["--interactive", "--foo", "--bar"]

      it "disables magic mode" $ do
        runMagicMode <$> parseOptions options `shouldBe` Result False

      it "filters out --interactive" $ do
        ghcOptions . runConfig <$> parseOptions options `shouldBe` Result ["--foo", "--bar"]

      it "accepts --fast" $ do
        fastMode . runConfig <$> parseOptions ("--fast" : options) `shouldBe` Result True

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
          fastMode . runConfig <$> parseOptions [] `shouldBe` Result False

      context "with --fast" $ do
        it "enables fast mode" $ do
          fastMode . runConfig <$> parseOptions ["--fast"] `shouldBe` Result True

    describe "--preserve-it" $ do
      context "without --preserve-it" $ do
        it "does not preserve the `it` variable" $ do
          preserveIt . runConfig <$> parseOptions [] `shouldBe` Result False

      context "with --preserve-it" $ do
        it "preserves the `it` variable" $ do
          preserveIt . runConfig <$> parseOptions ["--preserve-it"] `shouldBe` Result True

    describe "--fail-fast" $ do
      context "without --fail-fast" $ do
        it "disables fail-fast mode" $ do
          failFast . runConfig <$> parseOptions [] `shouldBe` Result False

      context "with --fail-fast" $ do
        it "enables fail-fast mode" $ do
          failFast . runConfig <$> parseOptions ["--fail-fast"] `shouldBe` Result True

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
          verbose . runConfig <$> parseOptions [] `shouldBe` Result False

      context "with --verbose" $ do
        it "parses verbose option" $ do
          verbose . runConfig <$> parseOptions ["--verbose"] `shouldBe` Result True
