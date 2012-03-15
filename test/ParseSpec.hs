{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module ParseSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Data.String.Builder (Builder, build)
import           Control.Monad.Trans.Writer

import           Parse

main :: IO ()
main = hspecX spec

ghci :: String -> Builder -> Writer [Interaction] ()
ghci e = tell . return . Interaction e . lines . build

spec :: Specs
spec = do

  describe "getDocTests" $ do
    it "extracts examples from a module" $ do
      expected <- return . return . DocExample "Fib" . execWriter $ do
        ghci "putStrLn \"foo\""
          "foo"
        ghci "putStr \"bar\""
          "bar"
        ghci "putStrLn \"baz\""
          "baz"
      r <- getDocTests [] ["parse/simple/Fib.hs"]
      r `shouldBe` expected

    it "extracts examples from documentation for non-exported names" $ do
      expected <- return . return . DocExample "Fib" . execWriter $ do
        ghci "putStrLn \"foo\""
          "foo"
        ghci "putStr \"bar\""
          "bar"
        ghci "putStrLn \"baz\""
          "baz"
      r <- getDocTests [] ["parse/non-exported/Fib.hs"]
      r `shouldBe` expected

    it "returns an empty list, if documentation contains no examples" $ do
      r <- getDocTests [] ["parse/no-examples/Fib.hs"]
      r `shouldBe` []

  describe "parse (an internal function)" $ do
    it "parses an interaction" $ do
      parse . build $ do
        ">>> foo"
        "23"
      `shouldBe` [Interaction "foo" ["23"]]

    it "drops whitespace as appropriate" $ do
      parse . build $ do
        "    >>> foo   "
        "    23"
      `shouldBe` [Interaction "foo" ["23"]]

    it "parses an interaction without a result" $ do
      parse . build $ do
        ">>> foo"
      `shouldBe` [Interaction "foo" []]

    it "works with a complex example" $ do
      parse . build $ do
        "test"
        "foobar"
        ""
        ">>> foo"
        "23"
        ""
        ">>> baz"
        ""
        ">>> bar"
        "23"
        ""
        "baz"
      `shouldBe` [Interaction "foo" ["23"], Interaction "baz" [], Interaction "bar" ["23"]]
