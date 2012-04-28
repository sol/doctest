module UtilSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Util

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "convertDosLineEndings" $ do
    it "converts CRLF to LF" $ do
      convertDosLineEndings "foo\r\nbar\r\nbaz" `shouldBe` "foo\nbar\nbaz"

    it "strips a trailing CR" $ do
      convertDosLineEndings "foo\r" `shouldBe` "foo"
