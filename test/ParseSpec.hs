{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module ParseSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit
import           Data.String.Builder (Builder, build)
import           Control.Monad.Trans.Writer

import           Parse
import           Location

main :: IO ()
main = hspecX spec

ghci :: String -> Builder -> Writer [Interaction] ()
ghci e = tell . return . Interaction e . lines . build

example :: Writer [Interaction] () -> Writer [DocTest] ()
example = tell . return . DocExample . execWriter

module_ :: String -> Writer [DocTest] () -> Writer [Module DocTest] ()
module_ name = tell . return . Module name . execWriter

shouldGive :: IO [Module DocTest] -> Writer [Module DocTest] () -> Assertion
shouldGive action w = action >>= (`shouldBe` execWriter w)

spec :: Specs
spec = do

  describe "getDocTests" $ do
    it "extracts examples from a module" $ do
      getDocTests [] ["test/parse/simple/Fib.hs"] `shouldGive` do
        module_ "Fib" $ do
          example $ do
            ghci "putStrLn \"foo\""
              "foo"
            ghci "putStr \"bar\""
              "bar"
            ghci "putStrLn \"baz\""
              "baz"

    it "extracts examples from documentation for non-exported names" $ do
      getDocTests [] ["test/parse/non-exported/Fib.hs"] `shouldGive` do
        module_ "Fib" $ do
          example $ do
            ghci "putStrLn \"foo\""
              "foo"
            ghci "putStr \"bar\""
              "bar"
            ghci "putStrLn \"baz\""
              "baz"

    it "extracts multiple examples from a module" $ do
      getDocTests [] ["test/parse/multiple-examples/Foo.hs"] `shouldGive` do
        module_ "Foo" $ do
          example $ do
            ghci "foo"
              "23"
          example $ do
            ghci "bar"
              "42"

    it "returns an empty list, if documentation contains no examples" $ do
      getDocTests [] ["test/parse/no-examples/Fib.hs"] >>= (`shouldBe` [])

  describe "parse (an internal function)" $ do

    let parse_ = parse . noLocation . build

    it "parses an interaction" $ do
      parse_ $ do
        ">>> foo"
        "23"
      `shouldBe` [Interaction "foo" ["23"]]

    it "drops whitespace as appropriate" $ do
      parse_ $ do
        "    >>> foo   "
        "    23"
      `shouldBe` [Interaction "foo" ["23"]]

    it "parses an interaction without a result" $ do
      parse_ $ do
        ">>> foo"
      `shouldBe` [Interaction "foo" []]

    it "works with a complex example" $ do
      parse_ $ do
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