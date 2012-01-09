module DocTest (
    getDocTests
  , DocTest(..)
  , Interaction(..)
  , toTestCase
  , toAssertion
  ) where

import Test.HUnit (Test(..), assertEqual, Assertion)
import qualified Interpreter
import Parse


toTestCase :: Interpreter.Interpreter -> DocTest -> Test
toTestCase repl test = TestLabel sourceFile $ TestCase $ toAssertion repl test
  where
    -- FIXME: use source location here
    sourceFile = moduleName test

-- |
-- Execute all expressions from given 'DocTest' in given
-- 'Interpreter.Interpreter' and verify the output.
--
-- The interpreter state is zeroed with @:reload@ before executing the
-- expressions.  This means that you can reuse the same
-- 'Interpreter.Interpreter' for several calls to `toAssertion`.
toAssertion :: Interpreter.Interpreter -> DocTest -> Assertion
toAssertion repl test = do
  _ <- Interpreter.eval repl $ ":reload"
  _ <- Interpreter.eval repl $ ":m *" ++ moduleName test
  mapM_ interactionToAssertion $ interactions test
  where
    interactionToAssertion x = do
      result' <- Interpreter.eval repl exampleExpression
      assertEqual ("expression `" ++ exampleExpression ++ "'")
        exampleResult $ lines result'
      where
        exampleExpression = expression x
        exampleResult     = result x
