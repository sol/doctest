module DocTest.DocTest where

import Test.HUnit (Test(..), assertEqual)
import System.FilePath (pathSeparator, dropExtension)
import System.Directory (canonicalizePath)
import DocTest.Util (stripPostfix, replace)
import System.Process (readProcess)
import GHC.Paths ( ghc )
import Documentation.Haddock.DocTest (DocTest(..))

docTestToTestCase :: DocTest -> IO Test
docTestToTestCase test = do
  canonicalModulePath <- canonicalizePath $ source test
  let baseDir = packageBaseDir canonicalModulePath (module_ test)
  result' <- runInterpreter ["-i" ++ baseDir, source test] $ expression test
  return (TestCase $ assertEqual (source test)
    (strip' $ unlines $ result test)
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
runInterpreter flags expr = do
  readProcess ghc myFlags expr
  where
    myFlags = ["-v0", "--interactive", "-ignore-dot-ghci"] ++ flags

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
