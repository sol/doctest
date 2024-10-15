{-# LANGUAGE CPP, OverloadedStrings #-}
module RunnerSpec (spec) where

import           Imports

import           Test.Hspec

import           Data.IORef
import           System.IO
import           System.IO.Silently (hCapture_)
import           Runner

capture :: Interactive -> Report () -> IO String
capture interactive action = do
  ref <- newIORef mempty
  hCapture_ [stderr] (runReport (ReportState interactive NonVerbose ref) action)

spec :: Spec
spec = do
  describe "report" $ do
    context "when mode is interactive" $ do
      it "writes to stderr" $ do
        capture Interactive $ do
          report "foobar"
        `shouldReturn` "foobar\n"

    context "when mode is non-interactive" $ do
      it "writes to stderr" $ do
        capture NonInteractive $ do
          report "foobar"
        `shouldReturn` "foobar\n"

  describe "report_" $ do
    context "when mode is interactive" $ do
      it "writes transient output to stderr" $ do
        capture Interactive $ do
          reportTransient "foobar"
        `shouldReturn` "foobar\r      \r"

    context "when mode is non-interactive" $ do
      it "is ignored" $ do
        capture NonInteractive $ do
          reportTransient "foobar"
        `shouldReturn` ""
