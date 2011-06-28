module Test.DocTest (
    DocTest
  , getDocTests
  , sourcePath
  , firstExpression
  , toAssertion
  , Interpreter
  , withInterpreter
  ) where

import           HaddockBackend.Api
import           DocTest
import           Interpreter (Interpreter)
import qualified Interpreter

sourcePath :: DocTest -> FilePath
sourcePath = source

firstExpression :: DocTest -> String
firstExpression test = expression $ head $ interactions test

withInterpreter
  :: [String]               -- ^ List of flags, passed to GHC
  -> (Interpreter -> IO a)  -- ^ Action to run
  -> IO a                   -- ^ Result of action
withInterpreter = Interpreter.withInterpreter
