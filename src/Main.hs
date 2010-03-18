module Main where

import System.Environment   ( getArgs )
import DocTest.DocTest      ( DocTest(..)
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

docTestFromAsset :: DocTestAsset -> [DocTest]
docTestFromAsset asset = map tranform (testList asset)
  where
    tranform :: Example -> DocTest
    tranform (Example p e r) = DocTest
            { source = sourceFile asset
            , _module = moduleName asset
            , expression = e
            , result = unlines r
            }

haddockParse args = do
  testAssets <- getTestAssets args
  return $ concat $ map docTestFromAsset testAssets

main = do
  args <- getArgs
  docTests <- haddockParse args
  tests <- mapM docTestToTestCase docTests
  runTestTT (TestList tests)
