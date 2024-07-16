module Cabal.PathsSpec (spec) where

import           Imports

import           Test.Hspec

import           System.Directory

import           Cabal.Paths

spec :: Spec
spec = do
  describe "paths" $ do
    it "returns the path to 'ghc'" $ do
      (paths "cabal" >>= doesFileExist . ghc) `shouldReturn` True

    it "returns the path to 'ghc-pkg'" $ do
      (paths "cabal" >>= doesFileExist . ghcPkg) `shouldReturn` True

    it "returns the path to Cabal's cache directory" $ do
      (paths "cabal" >>= doesDirectoryExist . cache) `shouldReturn` True
