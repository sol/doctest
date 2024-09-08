{-# LANGUAGE CPP, OverloadedStrings #-}
module RunnerSpec (main, spec) where

import           Imports

import           Test.Hspec

import           System.IO
import           System.IO.Silently (hCapture)
import           Control.Monad.Trans.State
import           Runner

main :: IO ()
main = hspec spec

capture :: Report a -> IO String
capture = fmap fst . hCapture [stderr] . (`execStateT` ReportState True False mempty)

-- like capture, but with interactivity set to False
capture_ :: Report a -> IO String
capture_ = fmap fst . hCapture [stderr] . (`execStateT` ReportState False False mempty)

spec :: Spec
spec = do
  describe "report" $ do
    context "when mode is interactive" $ do
      it "writes to stderr" $ do
        capture $ do
          report "foobar"
        `shouldReturn` "foobar\n"

    context "when mode is non-interactive" $ do
      it "writes to stderr" $ do
        capture_ $ do
          report "foobar"
        `shouldReturn` "foobar\n"

  describe "report_" $ do
    context "when mode is interactive" $ do
      it "writes transient output to stderr" $ do
        capture $ do
          reportTransient "foobar"
        `shouldReturn` "foobar\r      \r"

    context "when mode is non-interactive" $ do
      it "is ignored" $ do
        capture_ $ do
          reportTransient "foobar"
        `shouldReturn` ""
