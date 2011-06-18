{-| Interface for extracting and executing doctests.

Use 'getDocTests' to extract the 'DocTest' examples from the Haddock
comments. To verify that the examples work turn the 'DocTest' examples
into 'Test.HUnit.Test' test cases with the help of
'Interpreter.withInterpreter' and 'toTestCase'. After this just wrap
the newly minted 'Test.HUnit.Test' cases into something suitable for
your favorite test framework. If you need a plain
'Test.HUnit.Assertion' use 'toAssertion'.

-}

module DocTest (
  getDocTests
  , DocTest(..)
  , Interaction(..)
  , toTestCase
  , toAssertion
  , Interpreter.withInterpreter
  , Interpreter.Interpreter
  ) where

import Test.HUnit (Test(..), assertEqual, Assertion)
import qualified Interpreter
import HaddockBackend.Api

{-| Execute the steps in 'DocTest' in 'Interpreter.Interpreter'

The state of the 'Interpreter.Interpreter' is zeroed with @:reload@
before executing the 'DocTest'. This means that you can run more than
one example using the same 'Interpreter.Interpreter' without
interference.

-}

toTestCase :: Interpreter.Interpreter -> DocTest -> Test
toTestCase repl test = TestLabel sourceFile $ TestCase $ toAssertion repl test
  where
    sourceFile = source test

toAssertion :: Interpreter.Interpreter -> DocTest -> Assertion
toAssertion repl test = do
  _ <- Interpreter.eval repl $ ":m *" ++ moduleName
  _ <- Interpreter.eval repl $ ":reload"
  mapM_ interactionToAssertion $ interactions test
  where
    moduleName = module_ test
    interactionToAssertion x = do
      result' <- Interpreter.eval repl exampleExpression
      assertEqual ("expression `" ++ exampleExpression ++ "'")
        exampleResult $ lines result'
      where
        exampleExpression = expression x
        exampleResult     = map subBlankLines $ result x

        -- interpret lines that only contain the string "<BLANKLINE>" as an
        -- empty line
        subBlankLines "<BLANKLINE>" = ""
        subBlankLines line          = line
