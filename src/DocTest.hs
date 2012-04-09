module DocTest (
  -- * Extracting examples from module
    getDocTests
  -- * Data types
  , Module(..)
  , Example
  , Interaction(..)
  -- * Helper functions
  , exampleToInteractions
  -- * Interaction tests with GHCi.
  , Interpreter.Interpreter
  , toTestCase
  , toAssertion
  ) where

import           Test.HUnit (Test(..), assertEqual, Assertion)

import qualified Interpreter
import           Parse
import           Location

-- |
-- Extract 'Interaction's from 'Example'.
exampleToInteractions :: Example -> [Interaction]
exampleToInteractions (Example lis) = map unlocated lis
  where
    unlocated (Located _ inter) = inter

-- |
-- A wrapper function for 'toAssertion'
toTestCase :: Interpreter.Interpreter -> Module Example -> Test
toTestCase repl (Module name examples) = TestLabel name . TestList . map (TestCase . toAssertion repl name) $ examples

-- |
-- Execute all 'Interaction's from given 'Example' in given
-- 'Interpreter.Interpreter' and verify the output.
--
-- The interpreter state is zeroed with @:reload@ before executing the
-- expressions.  This means that you can reuse the same
-- 'Interpreter.Interpreter' for several calls to `toAssertion`.
toAssertion :: Interpreter.Interpreter -> String -> Example -> Assertion
toAssertion repl module_ (Example interactions) = do
  -- A module is alreay loaded here
  -- Clear interpreter status.
  _ <- Interpreter.eval repl ":reload"
  -- Fix name space.
  _ <- Interpreter.eval repl $ ":m *" ++ module_
  mapM_ interactionToAssertion interactions
  where
    interactionToAssertion (Located loc x) = do
      result' <- Interpreter.eval repl exampleExpression
      assertEqual (show loc ++ ": expression `" ++ exampleExpression ++ "'")
        exampleResult $ lines result'
      where
        exampleExpression = expression x
        exampleResult     = result x
