-- |
-- An experimental API for extracting and executing `DocTest`s.
--
-- Use 'getDocTests' to extract interaction examples from Haddock comments.
-- One 'Example' is for one function and contains all interaction examples for
-- the function.
-- Use 'runModules' or 'runExample' to test.
module Test.DocTest (
    -- * Extracting examples from module
    getDocTests
    -- * Data types
  , Module(..)
  , Example
    -- * Helper functions
  , exampleLabel
    -- * Interaction tests with GHCi.
  , Interpreter
  , newInterpreter
  , closeInterpreter
  , withInterpreter
  , eval
    -- * Running tests
  , runModules
  , runExample
  , InteractionResult(..)
  ) where

import Interpreter
import Parse
import Report
