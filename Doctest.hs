--module Doctest where

import Control.Exception
import Test.HUnit
import System.Plugins


loadTest :: String -> IO Test
loadTest file = do
	mv <- load file ["/home/sol/projects/wiktory/wyng/dep/Doctest"] [] "docTest"
	case mv of
		LoadFailure msg -> fail ("error while loading " ++ file)
		LoadSuccess _ v -> return v



-- Example:
--
-- > putStrLn (makeTest "Fib.hs - line 6: " "fib 10" "55")
-- > makeTest "Fib.hs - line 6: " "fib 10" "55"
-- "mytest = TestCase (assertEqual \"Fib.hs - line 6: \" (show (fib 10)) \"55\")"
makeTest source expression result = "mytest = TestCase (assertEqual \"" ++ source ++ "\" " ++ "(show (" ++ expression ++ "))" ++ " \"" ++ result ++ "\")"



main = do
	tc <- loadTest "Fibtest.o"
	runTestTT (TestList [tc])
