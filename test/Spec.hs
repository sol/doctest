module Main (main) where

import           Test.Hspec.ShouldBe

import qualified ExtractSpec
import qualified ParseSpec
import qualified InterpreterSpec
import qualified MainSpec

main :: IO ()
main = hspecX $ do
  describe "ExtractSpec"      ExtractSpec.spec
  describe "ParseSpec"        ParseSpec.spec
  describe "InterpreterSpec"  InterpreterSpec.spec
  describe "MainSpec"         MainSpec.spec
