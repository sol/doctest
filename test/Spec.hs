module Spec (main) where

import           Test.Hspec.ShouldBe

import qualified ExtractSpec
import qualified ParseSpec
import qualified InterpreterSpec

main :: IO ()
main = hspecX $ do
  describe "ExtractSpec"      ExtractSpec.spec
  describe "ParseSpec"        ParseSpec.spec
  describe "InterpreterSpec"  InterpreterSpec.spec
