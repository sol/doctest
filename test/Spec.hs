module Main (main) where

import           Test.Hspec.ShouldBe

import qualified UtilSpec
import qualified LocationSpec
import qualified ExtractSpec
import qualified ParseSpec
import qualified InterpreterSpec
import qualified ReportSpec
import qualified MainSpec

main :: IO ()
main = hspecX $ do
  describe "UtilSpec"         UtilSpec.spec
  describe "LocationSpec"     LocationSpec.spec
  describe "ExtractSpec"      ExtractSpec.spec
  describe "ParseSpec"        ParseSpec.spec
  describe "InterpreterSpec"  InterpreterSpec.spec
  describe "ReportSpec"       ReportSpec.spec
  describe "MainSpec"         MainSpec.spec
