-- Copyright (c) 2009 Simon Hengel <simon.hengel@web.de>
module Test.DocTest where

import Control.Exception
import Test.HUnit
import System.Plugins
import System.IO
import System.FilePath


loadTest :: String -> IO Test
loadTest file = do
	mv <- load file ["/home/sol/projects/Doctest"] [] "docTest"
	case mv of
		LoadFailure msg -> fail ("error while loading " ++ file ++ ": " ++ (concat msg))
		LoadSuccess _ v -> return v


compileModule filename = do
	status <- makeAll filename []
	case status of
		 MakeFailure errors -> fail (concat errors)
		 MakeSuccess _ file -> return file


-- Example:
--
-- > makeTest "Fib.hs - line 6: " "fib 10" "55"
-- "docTest = TestCase (assertEqual \"Fib.hs - line 6: \" (show (fib 10)) \"55\")"

makeTest (DocTest source _ expression result) = "docTest = TestCase (assertEqual \"" ++ source ++ "\" " ++ "(show (" ++ expression ++ "))" ++ " \"" ++ result ++ "\")"


data DocTest = DocTest {
	  source		:: String
	, _module		:: String
	, expression	:: String
	, result		:: String
	}
	deriving (Show)


writeModule test moduleName h = do
	hPutStrLn h ("module " ++ moduleName ++ " where")
	hPutStrLn h "import Test.HUnit"
	hPutStrLn h "import Fib\n"
	hPutStrLn h (makeTest test)


doTest :: FilePath -> DocTest -> IO Test
doTest directory test = do
	let moduleName = (_module test) ++ "DocTest"
	let filename = combine directory (moduleName ++ ".hs")
	withFile filename WriteMode (writeModule test moduleName)
	obj_file <- compileModule filename
	loadTest obj_file
