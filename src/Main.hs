module Main where

import System.Environment   ( getArgs )
import Test.DocTest.Parser  ( parseModule )
import Test.DocTest         ( DocTest(..)
                            , docTestToTestCase
                            )
import Test.HUnit           ( runTestTT
                            , Test(..)
                            )
import Documentation.Haddock.DocTest  (
                              DocTestAsset(..)
                            , Example(..)
                            , getTestAssets
                            )
import Data.Char

-- | Remove all leading and trailing whitespace
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

docTestFromAsset :: DocTestAsset -> [DocTest]
docTestFromAsset asset = map tranform (testList asset)
  where
    tranform :: Example -> DocTest
    tranform (Example p e r) = DocTest
            { source = sourceFile asset
            , _module = moduleName asset
            , expression = e
            , result = strip $ unlines r
            }

internalParse args = do
  docTests <- mapM parseModule args
  return $ concat docTests

haddockParse args = do
  testAssets <- getTestAssets args
  return $ concat $ map docTestFromAsset testAssets

main = do
  args <- getArgs
  docTests <- haddockParse args
  tests <- mapM docTestToTestCase docTests
  runTestTT (TestList tests)
