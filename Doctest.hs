-- Copyright (c) 2009 Simon Hengel <simon.hengel@web.de>
module Doctest where

import Control.Exception
import Test.HUnit
import System.Plugins
import System.IO
import System.Directory
import Text.Parsec


loadTest :: String -> IO Test
loadTest file = do
	mv <- load file ["/home/sol/projects/wiktory/wyng/dep/Doctest"] [] "docTest"
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


writeModule test h = do
	hPutStrLn h "module Foo where"
	hPutStrLn h "import Test.HUnit"
	hPutStrLn h "import Fib\n"
	hPutStrLn h (makeTest test)


doTest :: DocTest -> IO Test
doTest test = do
	let filename = "Foo.hs"
	withFile filename WriteMode (writeModule test)
	obj_file <- compileModule filename
	loadTest obj_file

main = do
	tc <- doTest (DocTest "Fib.hs - line 6: " "Fib" "fib 10" "55")
	runTestTT (TestList [tc])


{-
simpleComment = do
	string "<!--"
	manyTill anyChar (Text.Parsec.try (string "-->"))
-}


