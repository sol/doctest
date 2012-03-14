-- |
-- An experimental API for extracting and executing `DocTest`s.
--
-- Use 'getDocTests' to extract 'DocTest' examples from Haddock comments.  To
-- verify that the examples work turn them into 'Test.HUnit.Assertion's, using
-- 'withInterpreter' and 'toAssertion'.  After this just wrap the newly minted
-- assertions into something suitable for your favorite test framework.
module Test.DocTest (
    DocTest
  , getDocTests
  , sourcePath
  , firstExpression
  , toAssertion
  , Interpreter
  , withInterpreter
  ) where

import           DocTest
import           Interpreter (Interpreter)
import qualified Interpreter

-- | Instead of what the name suggests, this returns the module name, not the
-- file name.
sourcePath :: DocTest -> FilePath
sourcePath = moduleName
{-# DEPRECATED sourcePath "this function will vanish in a future release" #-}

firstExpression :: DocTest -> String
firstExpression test = expression $ head $ interactions test

withInterpreter
  :: [String]               -- ^ List of flags, passed to GHC
  -> (Interpreter -> IO a)  -- ^ Action to run
  -> IO a                   -- ^ Result of action
withInterpreter = Interpreter.withInterpreter
