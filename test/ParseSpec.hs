{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module ParseSpec (main, spec) where

import           Test.Hspec
import           Data.String.Builder (Builder, build)
import           Control.Monad.Trans.Writer

import           Parse
import           Location

main :: IO ()
main = hspec spec

group :: Writer [DocTest] () -> Writer [[DocTest]] ()
group g = tell [execWriter g]

ghci :: Expression -> Builder -> Writer [DocTest] ()
ghci expressions expected = tell [Example expressions $ (lines . build) expected]

prop_ :: Expression -> Writer [DocTest] ()
prop_ e = tell [Property e]

module_ :: String -> Writer [[DocTest]] () -> Writer [Module [DocTest]] ()
module_ name gs = tell [Module name Nothing $ execWriter gs]

shouldGive :: IO [Module [Located DocTest]] -> Writer [Module [DocTest]] () -> Expectation
shouldGive action expected = map (fmap $ map unLoc) `fmap` action `shouldReturn` execWriter expected

spec :: Spec
spec = do
  describe "getDocTests" $ do
    it "extracts properties from a module" $ do
      getDocTests ["test/parse/property/Fib.hs"] `shouldGive` do
        module_ "Fib" $ do
          group $ do
            prop_ "foo"
            prop_ "bar"
            prop_ "baz"

    it "extracts examples from a module" $ do
      getDocTests ["test/parse/simple/Fib.hs"] `shouldGive` do
        module_ "Fib" $ do
          group $ do
            ghci "putStrLn \"foo\""
              "foo"
            ghci "putStr \"bar\""
              "bar"
            ghci "putStrLn \"baz\""
              "baz"

    it "extracts examples from documentation for non-exported names" $ do
      getDocTests ["test/parse/non-exported/Fib.hs"] `shouldGive` do
        module_ "Fib" $ do
          group $ do
            ghci "putStrLn \"foo\""
              "foo"
            ghci "putStr \"bar\""
              "bar"
            ghci "putStrLn \"baz\""
              "baz"

    it "extracts multiple examples from a module" $ do
      getDocTests ["test/parse/multiple-examples/Foo.hs"] `shouldGive` do
        module_ "Foo" $ do
          group $ do
            ghci "foo"
              "23"
          group $ do
            ghci "bar"
              "42"

    it "returns an empty list, if documentation contains no examples" $ do
      getDocTests ["test/parse/no-examples/Fib.hs"] >>= (`shouldBe` [])

  describe "parseInteractions (an internal function)" $ do

    let parse_ = map unLoc . parseInteractions . noLocation . build

    it "parses an interaction" $ do
      parse_ $ do
        ">>> foo"
        "23"
      `shouldBe` [("foo", ["23"])]

    it "drops whitespace as appropriate" $ do
      parse_ $ do
        "    >>> foo   "
        "    23"
      `shouldBe` [("foo", ["23"])]

    it "parses an interaction without a result" $ do
      parse_ $ do
        ">>> foo"
      `shouldBe` [("foo", [])]

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
      `shouldBe` [("foo", ["23"]), ("baz", []), ("bar", ["23"])]

    it "attaches location information to parsed interactions" $ do
      let loc = Located . Location "Foo.hs"
      r <- return . parseInteractions . loc 23 . build  $ do
        "1"
        "2"
        ""
        ">>> 4"
        "5"
        ""
        ">>> 7"
        ""
        ">>> 9"
        "10"
        ""
        "11"
      r `shouldBe` [loc 26 $ ("4", ["5"]), loc 29 $ ("7", []), loc 31 $ ("9", ["10"])]

  describe " parseProperties (an internal function)" $ do
    let parse_ = map unLoc . parseProperties . noLocation . build

    it "parses a property" $ do
      parse_ $ do
        "prop> foo"
      `shouldBe` ["foo"]
