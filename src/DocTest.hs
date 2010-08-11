module DocTest (getTest) where

import Test.HUnit (Test(..), assertEqual, Assertion)
import System.FilePath (pathSeparator, dropExtension)
import System.Directory (canonicalizePath)
import Util (stripPostfix, replace)
import HaddockBackend.Api
import qualified Interpreter
import Options

getTest :: [Option] -> [String] -> IO Test
getTest options files = do
  docTests <- getDocTests haddockArgs
  return $ TestList $ map (toTestCase $ ghcOptions options) docTests
  where
    haddockArgs = (haddockOptions options) ++ files


toTestCase :: [String] -> DocTest -> Test
toTestCase ghcFlags test = TestCase $ do
  let moduleName = module_ test
  modulePath <- canonicalizePath $ sourceFile
  let baseDir = packageBaseDir modulePath moduleName
  let ghcArgs = ghcFlags ++ ["-i" ++ baseDir, sourceFile]
  Interpreter.withInterpreter ghcArgs $ interactionsToAssertion $ interactions test
  where
    sourceFile        = source test

    interactionsToAssertion :: [Interaction] -> Interpreter.Interpreter -> Assertion
    interactionsToAssertion []     _    = return ()
    interactionsToAssertion (x:xs) repl = do
      result' <- Interpreter.eval repl exampleExpression
      assertEqual sourceFile
        (exampleResult)
        (lines result')
      interactionsToAssertion xs repl
      where
        exampleExpression = expression x
        exampleResult     = result x


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
