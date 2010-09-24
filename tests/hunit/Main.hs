module Main where

import Test.HUnit

import qualified TestInterpreter

main :: IO ()
main = do
  _ <- runTestTT $ TestInterpreter.tests
  return ();
