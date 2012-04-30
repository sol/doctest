module Main (main) where

import           Test.Hspec.ShouldBe

import           Run (doctest)

import qualified UtilSpec
import qualified LocationSpec
import qualified ExtractSpec
import qualified ParseSpec
import qualified InterpreterSpec
import qualified ReportSpec
import qualified PropertySpec
import qualified RunSpec
import qualified MainSpec

main :: IO ()
main = do
  doctest [
      "--optghc=-packageghc"
    , "--optghc=-isrc"
    , "--optghc=-idist/build/autogen/"
    , "--optghc=-optP-include"
    , "--optghc=-optPdist/build/autogen/cabal_macros.h"
    , "src/Run.hs"
    ]
  hspecX $ do
    describe "UtilSpec"         UtilSpec.spec
    describe "LocationSpec"     LocationSpec.spec
    describe "ExtractSpec"      ExtractSpec.spec
    describe "ParseSpec"        ParseSpec.spec
    describe "InterpreterSpec"  InterpreterSpec.spec
    describe "ReportSpec"       ReportSpec.spec
    describe "PropertySpec"     PropertySpec.spec
    describe "RunSpec"          RunSpec.spec
    describe "MainSpec"         MainSpec.spec
