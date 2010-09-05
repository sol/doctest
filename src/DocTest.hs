module DocTest (getTest) where

import Test.HUnit (Test(..), assertEqual, Assertion)
import HaddockBackend.Api
import qualified Interpreter
import Options

getTest :: [Option] -> [String] -> IO Test
getTest options files = do
  docTests <- getDocTests haddockFlags files
  return $ TestList $ map (toTestCase ghciArgs) docTests
  where
    ghciArgs = ghcOptions options ++ files
    haddockFlags = haddockOptions options


toTestCase :: [String] -> DocTest -> Test
toTestCase ghciArgs test = TestCase $ do
  Interpreter.withInterpreter ghciArgs $ \repl -> do
    -- bring module into scope before running tests..
    _ <- Interpreter.eval repl $ ":m *" ++ moduleName
    interactionsToAssertion (interactions test) repl
  where
    moduleName = module_ test
    sourceFile = source test

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
