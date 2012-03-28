module ReportSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Data.Monoid
import           System.IO
import           System.IO.Silently (hCapture)
import           Control.Monad.Trans.State
import           Report

main :: IO ()
main = hspecX spec

capture :: Report a -> IO String
capture = fmap fst . hCapture [stderr] . (`execStateT` ReportState 0 mempty)

spec :: Specs
spec = do

  describe "report" $ do
    it "writes to stderr" $ do
      capture $ do
        report "foobar"
      >>= (`shouldBe` "foobar\n")

    it "overwrites any intermediate output" $ do
      capture $ do
        report_ "foo"
        report  "bar"
      >>= (`shouldBe` "foo\rbar\n")

    it "blank out intermediate output if necessary" $ do
      capture $ do
        report_ "foobar"
        report  "baz"
      >>= (`shouldBe` "foobar\rbaz   \n")

  describe "report_" $ do
    it "writes intermediate output to stderr" $ do
      capture $ do
        report_ "foobar"
      >>= (`shouldBe` "foobar")

    it "overwrites any intermediate output" $ do
      capture $ do
        report_ "foo"
        report_ "bar"
      >>= (`shouldBe` "foo\rbar")

    it "blank out intermediate output if necessary" $ do
      capture $ do
        report_ "foobar"
        report_  "baz"
      >>= (`shouldBe` "foobar\rbaz   ")
