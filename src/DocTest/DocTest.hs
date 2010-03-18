module DocTest.DocTest where

import Test.HUnit
import System.FilePath
import System.Directory
import DocTest.Util
import System.Process
import GHC.Paths ( ghc )

data DocTest = DocTest {
  source      :: String
, _module     :: String
, expression  :: String
, result      :: String
} deriving (Show)

docTestToTestCase :: DocTest -> IO Test
docTestToTestCase test = do
  canonicalModulePath <- canonicalizePath $ source test
  let baseDir = packageBaseDir canonicalModulePath (_module test)
  result' <- runInterpreter ["-i" ++ baseDir, source test] $ expression test
  return (TestCase $ assertEqual (source test)
    (strip' $ result test)
    (strip' result')
    )
  where
    strip' = stripPostfix "\n"

-- | Evaluate given expression with ghci.
--
-- Examples:
-- ghci> runInterpreter [] "putStrLn \"foobar\""
-- "foobar\n"
-- ghci> runInterpreter [] "23 + 42"
-- "65\n"
runInterpreter :: [String] -> String -> IO String
runInterpreter flags expression = do
  readProcess ghc myFlags expression
  where
    myFlags = ["-v0", "--interactive"] ++ flags

-- | Map a given source file and a corresponding module name to the base
-- directory of the package.
--
-- Example:
-- ghci> packageBaseDir "/foo/bar/MyPackage/MyModule.hs" "MyPackage.MyModule"
-- "/foo/bar/"
-- ghci> packageBaseDir "foo" "bar"
-- "foo*** Exception: Prelude.undefined
packageBaseDir :: FilePath -> String -> FilePath
packageBaseDir moduleSrcFile moduleName = stripPostfix (replace "." [pathSeparator] moduleName) (dropExtension moduleSrcFile)
