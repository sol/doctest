{-# LANGUAGE CPP #-}

module LocationSpec (main, spec) where

import           Imports

import           Test.Hspec

import           Location

#if __GLASGOW_HASKELL__ < 900
import           SrcLoc
import           FastString (fsLit)
#else
import           GHC.Types.SrcLoc
import           GHC.Data.FastString (fsLit)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "toLocation" $ do

    it "works for a regular SrcSpan" $ do
      toLocation (mkSrcSpan (mkSrcLoc (fsLit "Foo.hs") 2 5) (mkSrcLoc (fsLit "Foo.hs") 10 20))
        `shouldBe` Location "Foo.hs" 2

    it "works for a single-line SrcSpan" $ do
      toLocation (mkSrcSpan (mkSrcLoc (fsLit "Foo.hs") 2 5) (mkSrcLoc (fsLit "Foo.hs") 2 10))
        `shouldBe` Location "Foo.hs" 2

    it "works for a SrcSpan that corresponds to single point" $ do
      (toLocation . srcLocSpan) (mkSrcLoc (fsLit "Foo.hs") 10 20)
        `shouldBe` Location "Foo.hs" 10

    it "works for a bad SrcSpan" $ do
      toLocation noSrcSpan `shouldBe` UnhelpfulLocation "<no location info>"

    it "works for a SrcLoc with bad locations" $ do
      toLocation (mkSrcSpan noSrcLoc noSrcLoc)
        `shouldBe` UnhelpfulLocation "<no location info>"

  describe "enumerate" $ do
    it "replicates UnhelpfulLocation" $ do
      let loc = UnhelpfulLocation "foo"
      (take 10 $ enumerate loc) `shouldBe` replicate 10 loc

    it "enumerates Location" $ do
      let loc = Location "Foo.hs" 23
      (take 3 $ enumerate loc) `shouldBe` [Location "Foo.hs" 23, Location "Foo.hs" 24, Location "Foo.hs" 25]
