module SandboxSpec where

import Test.Hspec

import Sandbox

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getPackageDbDir" $ do
    it "parses a config file and extracts package db" $ do
      pkgDb <- getPackageDbDir "test/sandbox/cabal.sandbox.config"
      pkgDb `shouldBe` "/home/me/doctest-haskell/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"

    it "throws an error if a config file is broken" $ do
      getPackageDbDir "test/sandbox/bad.config" `shouldThrow` anyException
