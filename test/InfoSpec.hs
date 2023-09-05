module InfoSpec (spec) where

import           Imports

import           Test.Hspec

import           System.Process

import           Info (formatInfo)
import           Interpreter (ghc)

spec :: Spec
spec = do
  describe "formatInfo" $ do
    it "formats --info output" $ do
      info <- readProcess ghc ["--info"] ""
      formatInfo (read info) `shouldBe` info
