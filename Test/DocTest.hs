-- Copyright (c) 2009 Simon Hengel <simon.hengel@web.de>
module Test.DocTest where

import Test.HUnit
import System.FilePath
import System.Directory
import Test.DocTest.Util
import System.Process


data DocTest = DocTest {
	  source		:: String
	, _module		:: String
	, expression	:: String
	, result		:: String
	}
	deriving (Show)


_testModule :: DocTest -> String
_testModule (DocTest source _module expression result) =
	"import " ++ _module  ++ "\n" ++
	"main = do\n" ++
	"    putStr (show (" ++ expression ++ "))\n"

runDocTest test baseDir = do
	ret <- readProcess "runhaskell" ["-i" ++ baseDir] (_testModule test)
	return (TestCase (assertEqual (source test) (result test) ret))


doTest :: FilePath -> DocTest -> IO Test
doTest directory test = do
	canonicalModulePath <- canonicalizePath $ source test
	let baseDir = packageBaseDir canonicalModulePath (_module test)
	runDocTest test baseDir

-- Maps a given source file and a corresponding module name to the base
-- directory of the package.
--
-- Example:
-- > packageBaseDir "/foo/bar/MyPackage/MyModule.hs" "MyPackage.MyModule"
-- "/foo/bar/"

-- > packageBaseDir "foo" "bar"
-- "foo*** Exception: Prelude.undefined

packageBaseDir :: FilePath -> String -> FilePath
packageBaseDir moduleSrcFile moduleName = stripPostfix (dropExtension moduleSrcFile) (replace "." [pathSeparator] moduleName)
