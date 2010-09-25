module Main where

import Test.HUnit (runTestTT, Test(..), assertEqual)

import HaddockBackend.Api
import Options

import qualified Interpreter

main :: IO ()
main = do
  (options, files) <- getOptions
  let ghciArgs = ghcOptions options ++ files
  Interpreter.withInterpreter ghciArgs $ \repl -> do

    -- get examples from Haddock comments
    let haddockFlags = haddockOptions options
    docTests <- getDocTests haddockFlags files

    if DumpOnly `elem` options
      then do
        -- dump to stdout
        print docTests
      else do
        -- map to unit tests
        let tests = TestList $ map (toTestCase repl) docTests
        _ <- runTestTT tests
        return ()

toTestCase :: Interpreter.Interpreter -> DocTest -> Test
toTestCase repl test = TestLabel sourceFile $ TestCase $ do
  -- bring module into scope before running tests..
  _ <- Interpreter.eval repl $ ":m *" ++ moduleName
  _ <- Interpreter.eval repl $ ":reload"
  mapM_ interactionToAssertion $ interactions test
  where
    moduleName = module_ test
    sourceFile = source test
    interactionToAssertion x = do
      result' <- Interpreter.eval repl exampleExpression
      assertEqual ("expression `" ++ exampleExpression ++ "'")
        exampleResult $ lines result'
      where
        exampleExpression = expression x
        exampleResult     = result x
