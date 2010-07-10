module DocTest (getTest) where

import Test.HUnit (Test(..), assertEqual, Assertion)
import System.FilePath (pathSeparator, dropExtension)
import System.Directory (canonicalizePath)
import Util (stripPostfix, replace)
import System.Process (readProcess)
import GHC.Paths ( ghc )
import HaddockBackend.Api (DocTest(..), getDocTests)

getTest :: [String] -> IO Test
getTest args = do
  docTests <- getDocTests args
  return $ TestList $ map toTestCase docTests

toTestCase :: DocTest -> Test
toTestCase test = TestCase $ toAssertion test

toAssertion :: DocTest -> Assertion
toAssertion test = do
  modulePath <- canonicalizePath $ sourceFile
  let baseDir = packageBaseDir modulePath moduleName
  result' <- runInterpreter ["-i" ++ baseDir, sourceFile] $ exampleExpression
  assertEqual sourceFile
    (exampleResult)
    (lines result')
  where
    sourceFile        = source test
    moduleName        = module_ test
    exampleExpression = expression test
    exampleResult     = result test


-- | Evaluate given expression with ghci.
--
-- Examples:
--
-- ghci> runInterpreter [] "putStrLn \"foobar\""
-- "foobar\n"
--
-- ghci> runInterpreter [] "23 + 42"
-- "65\n"
runInterpreter :: [String] -> String -> IO String
runInterpreter flags expr = do
  readProcess ghc myFlags expr
  where
    myFlags = ["-v0", "--interactive", "-ignore-dot-ghci"] ++ flags

-- | Map a given source file and a corresponding module name to the base
-- directory of the package.
--
-- Example:
--
-- ghci> packageBaseDir "/foo/bar/MyPackage/MyModule.hs" "MyPackage.MyModule"
-- "/foo/bar/"
--
-- ghci> packageBaseDir "foo" "bar"
-- "foo*** Exception: Prelude.undefined
packageBaseDir :: FilePath -> String -> FilePath
packageBaseDir moduleSrcFile moduleName = stripPostfix (replace "." [pathSeparator] moduleName) (dropExtension moduleSrcFile)
