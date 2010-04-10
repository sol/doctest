module Main where

import System.Environment   ( getArgs )
import DocTest.DocTest (docTestToTestCase)
import Test.HUnit           ( runTestTT
                            , Test(..)
                            )
import Documentation.Haddock.DocTest (getDocTests)

main :: IO ()
main = do
  args <- getArgs
  docTests <- getDocTests args
  tests <- mapM docTestToTestCase docTests
  _ <- runTestTT (TestList tests)
  return ()
