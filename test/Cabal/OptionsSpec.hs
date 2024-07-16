{-# LANGUAGE CPP #-}
module Cabal.OptionsSpec (spec) where

import           Imports

import           Test.Hspec

import           System.IO
import           System.IO.Silently
import           System.Exit
import           System.Process
import qualified Data.Set as Set

import           Cabal.Options

spec :: Spec
spec = do
  describe "pathOptions" $ do
    it "is the set of options that are unique to 'cabal path'" $ do
      build <- Set.fromList . lines <$> readProcess "cabal" ["build", "--list-options"] ""
      path <- Set.fromList . lines <$> readProcess "cabal" ["path", "--list-options"] ""
      map optionName pathOptions `shouldMatchList` Set.toList (Set.difference path build)

  describe "replOptions" $ do
    it "is the set of options that are unique to 'cabal repl'" $ do
      build <- Set.fromList . lines <$> readProcess "cabal" ["build", "--list-options"] ""
      repl <- Set.fromList . lines <$> readProcess "cabal" ["repl", "--list-options"] ""
      map optionName replOptions `shouldMatchList` Set.toList (Set.difference repl build)

  describe "rejectUnsupportedOptions" $ do
    it "produces error messages that are consistent with 'cabal repl'" $ do
      let
        shouldFail :: HasCallStack => String -> IO a -> Expectation
        shouldFail command action = do
          hCapture_ [stderr] (action `shouldThrow` (== ExitFailure 1))
            `shouldReturn` "Error: cabal: unrecognized '" <> command <> "' option `--installdir'\n"

#ifndef mingw32_HOST_OS
      shouldFail "repl" $ rawSystem "cabal" ["repl", "--installdir"] >>= exitWith
#endif
      shouldFail "doctest" $ rejectUnsupportedOptions ["--installdir"]

  describe "shouldReject" $ do
    it "accepts --foo" $ do
      shouldReject "--foo" `shouldBe` False

    it "rejects --ignore-project" $ do
      shouldReject "--ignore-project" `shouldBe` True

    it "rejects -z" $ do
      shouldReject "-z" `shouldBe` True

    it "rejects --output-format" $ do
      shouldReject "--output-format" `shouldBe` True

    it "rejects --output-format=" $ do
      shouldReject "--output-format=json" `shouldBe` True

    it "rejects --installdir" $ do
      shouldReject "--installdir" `shouldBe` True

  describe "discardReplOptions" $ do
    it "discards 'cabal repl'-only options" $ do
      discardReplOptions [
          "--foo"
        , "--build-depends=foo"
        , "--build-depends", "foo"
        , "-bfoo"
        , "-b", "foo"
        , "--bar"
        , "--enable-multi-repl"
        , "--repl-options", "foo"
        , "--repl-options=foo"
        , "--baz"
        ] `shouldBe` ["--foo", "--bar", "--baz"]

  describe "shouldDiscard" $ do
    it "keeps --foo" $ do
      shouldDiscard "--foo" `shouldBe` Keep

    it "discards --build-depends" $ do
      shouldDiscard "--build-depends" `shouldBe` DiscardWithArgument

    it "discards --build-depends=" $ do
      shouldDiscard "--build-depends=foo" `shouldBe` Discard

    it "discards -b" $ do
      shouldDiscard "-b" `shouldBe` DiscardWithArgument

    it "discards -bfoo" $ do
      shouldDiscard "-bfoo" `shouldBe` Discard

    it "discards --repl-options" $ do
      shouldDiscard "--repl-options" `shouldBe` DiscardWithArgument

    it "discards --repl-options=" $ do
      shouldDiscard "--repl-options=foo" `shouldBe` Discard

    it "discards --enable-multi-repl" $ do
      shouldDiscard "--enable-multi-repl" `shouldBe` Discard
