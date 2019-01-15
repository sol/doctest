module OptionsSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec
import           Test.QuickCheck

import           Options

spec :: Spec
spec = do
  describe "parseOptions" $ do
    let warning = ["WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."]
    it "strips --optghc" $
      property $ \xs ys ->
        parseOptions (xs ++ ["--optghc", "foobar"] ++ ys) `shouldBe` Result (Run warning (xs ++ ["foobar"] ++ ys) defaultMagic defaultFastMode defaultPreserveIt defaultInterpret defaultVerbose)

    it "strips --optghc=" $
      property $ \xs ys ->
        parseOptions (xs ++ ["--optghc=foobar"] ++ ys) `shouldBe` Result (Run warning (xs ++ ["foobar"] ++ ys) defaultMagic defaultFastMode defaultPreserveIt defaultInterpret defaultVerbose)

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

    describe "--no-interpret" $ do
      context "without --no-interpret" $ do
        it "interprets modules" $ do
          runInterpret <$> parseOptions [] `shouldBe` Result True

      context "with --no-interpret" $ do
        it "loads modules" $ do
          runInterpret <$> parseOptions ["--no-interpret"] `shouldBe` Result False

        it "disables magic mode" $ do
          runMagicMode <$> parseOptions ["--no-interpret"] `shouldBe` Result False

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
