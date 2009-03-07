module Main where

import System.Directory
import System.Environment
import System.FilePath
import Control.Exception(finally)
import Test.DocTest
import Test.DocTest.Parser
import Test.HUnit


main = do
	withTempDir "DocTestSandbox" run

run tmpdir = do
	args <- getArgs
	docTests <- mapM parseModule args
	tests <- mapM (doTest tmpdir) (concat docTests)
	runTestTT (TestList tests)

withTempDir :: FilePath -> (FilePath -> IO a) -> IO a
withTempDir name action = do
	tmpdir <- catch getTemporaryDirectory (\_ -> return ".")
	let path = combine tmpdir name
	--createDirectory path
	--finally (action path) (removeDirectoryRecursive path)
	createDirectoryIfMissing False path
	action path
