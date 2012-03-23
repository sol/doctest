module LocationSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Location

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "enumerate" $ do
    it "replicates UnhelpfulLocation" $ do
      let loc = UnhelpfulLocation "foo"
      (take 10 $ enumerate loc) `shouldBe` replicate 10 loc

    it "enumerates Location" $ do
      let loc = Location "Foo.hs" 23
      (take 3 $ enumerate loc) `shouldBe` [Location "Foo.hs" 23, Location "Foo.hs" 24, Location "Foo.hs" 25]
