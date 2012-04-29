{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module ReportSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Data.String.Builder
import           Test.HUnit

import           Data.Monoid
import           System.IO
import           System.IO.Silently (hCapture)
import           Control.Monad.Trans.State
import           Report
import           Interpreter (withInterpreter)
import           Location (noLocation)

main :: IO ()
main = hspecX spec

capture :: Report a -> IO String
capture = fmap fst . hCapture [stderr] . (`execStateT` ReportState 0 mempty)

shouldGive :: IO String -> Builder -> Assertion
action `shouldGive` expected = action `shouldReturn` build expected

spec :: Specs
spec = do

  describe "report" $ do
    it "writes to stderr" $ do
      capture $ do
        report "foobar"
      `shouldReturn` "foobar\n"

    it "overwrites any intermediate output" $ do
      capture $ do
        report_ "foo"
        report  "bar"
      `shouldReturn` "foo\rbar\n"

    it "blank out intermediate output if necessary" $ do
      capture $ do
        report_ "foobar"
        report  "baz"
      `shouldReturn` "foobar\rbaz   \n"

  describe "report_" $ do
    it "writes intermediate output to stderr" $ do
      capture $ do
        report_ "foobar"
      `shouldReturn` "foobar"

    it "overwrites any intermediate output" $ do
      capture $ do
        report_ "foo"
        report_ "bar"
      `shouldReturn` "foo\rbar"

    it "blank out intermediate output if necessary" $ do
      capture $ do
        report_ "foobar"
        report_  "baz"
      `shouldReturn` "foobar\rbaz   "

  describe "reportFailure" $ do
    it "works for one-line test output" $ do
      capture $ do
        reportFailure ["foo"] ["bar"]
      `shouldGive` do
        "expected: foo"
        " but got: bar"

    it "works for multi-line test output" $ do
      capture $ do
        reportFailure ["foo", "bar"] ["foo", "baz"]
      `shouldGive` do
        "expected: foo"
        "          bar"
        " but got: foo"
        "          baz"

    it "quotes output if any output line ends with trailing whitespace" $ do
      capture $ do
        reportFailure ["foo", "bar   "] ["foo", "baz"]
      `shouldGive` do
        "expected: \"foo\""
        "          \"bar   \""
        " but got: \"foo\""
        "          \"baz\""

    it "uses show to format output lines if any output line contains \"unsafe\" characters" $ do
      capture $ do
        reportFailure ["foo\160bar"] ["foo bar"]
      `shouldGive` do
        "expected: \"foo\\160bar\""
        " but got: \"foo bar\""

  describe "runProperty" $ do
    it "runs a Bool property" $ withInterpreter [] $ \repl -> do
      runProperty repl (noLocation "True") `shouldReturn` Success

    it "runs a Bool property with an explicit type signature" $ withInterpreter [] $ \repl -> do
      runProperty repl (noLocation "True :: Bool") `shouldReturn` Success
