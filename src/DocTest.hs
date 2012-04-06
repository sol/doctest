module DocTest (
    getDocTests
  , Example
  , Module(..)
  , Interaction(..)
  , exampleToInteractions
  , toTestCase
  , toAssertion
  ) where

import           Test.HUnit (Test(..), assertEqual, Assertion)

import qualified Interpreter
import           Parse
import           Location

exampleToInteractions :: Example -> [Interaction]
exampleToInteractions (Example lis) = map unlocated lis
  where
    unlocated (Located _ inter) = inter

toTestCase :: Interpreter.Interpreter -> Module Example -> Test
toTestCase repl (Module name examples) = TestLabel name . TestList . map (TestCase . toAssertion repl name) $ examples

-- |
-- Execute all expressions from given 'Example' in given
-- 'Interpreter.Interpreter' and verify the output.
toAssertion :: Interpreter.Interpreter -> String -> Example -> Assertion
toAssertion repl module_ (Example interactions) = do
  _ <- Interpreter.eval repl $ ":load " ++ module_
  mapM_ interactionToAssertion interactions
  where
    interactionToAssertion (Located loc x) = do
      result' <- Interpreter.eval repl exampleExpression
      assertEqual (show loc ++ ": expression `" ++ exampleExpression ++ "'")
        exampleResult $ lines result'
      where
        exampleExpression = expression x
        exampleResult     = result x
