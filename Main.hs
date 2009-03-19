module Main where

import System.Environment
import Test.DocTest
import Test.DocTest.Parser
import Test.HUnit

main = do
	args <- getArgs
	docTests <- mapM parseModule args
	tests <- mapM docTestToTestCase (concat docTests)
	runTestTT (TestList tests)
