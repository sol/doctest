module Test.DocTest (
    getDocTests
  , DocTest
  , source
  , interactions
  , expression
  , toAssertion
  , Interpreter.withInterpreter
  , Interpreter.Interpreter
  ) where

import HaddockBackend.Api
import DocTest
import qualified Interpreter
