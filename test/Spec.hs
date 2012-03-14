module Spec (main) where

import           Test.Hspec.ShouldBe

import qualified ExtractSpec
import qualified ParseSpec

main :: IO ()
main = hspecX $ do
  describe "ExtractSpec"  ExtractSpec.spec
  describe "ParseSpec"    ParseSpec.spec
