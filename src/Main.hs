module Main where

import System.Environment   ( getArgs )
import DocTest.DocTest      ( DocTest(..)
                            , docTestToTestCase
                            )
import Test.HUnit           ( runTestTT
                            , Test(..)
                            )
import Documentation.Haddock.DocTest  (
                              DocTestAsset
                            , assetModuleName
                            , assetSourceFile
                            , assetTestList
                            , Example
                            , exampleExpression
                            , exampleResult
                            , getTestAssets
                            )

docTestFromAsset :: DocTestAsset -> [DocTest]
docTestFromAsset asset = map tranform (assetTestList asset)
  where
    tranform :: Example -> DocTest
    tranform example = DocTest
            { source = assetSourceFile asset
            , _module = assetModuleName asset
            , expression = exampleExpression example
            , result = unlines $ exampleResult example
            }

haddockParse args = do
  testAssets <- getTestAssets args
  return $ concat $ map docTestFromAsset testAssets

main = do
  args <- getArgs
  docTests <- haddockParse args
  tests <- mapM docTestToTestCase docTests
  runTestTT (TestList tests)
