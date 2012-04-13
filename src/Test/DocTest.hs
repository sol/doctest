-- |
-- An experimental API for extracting and executing `DocTest`s.
--
-- Use 'getDocTests' to extract interaction examples from Haddock comments.
-- One 'Example' is for one function and contains all interaction examples for 
-- the function.
-- Use 'runModules' to test.
module Test.DocTest (
    -- * Extracting examples from module
    getDocTests
    -- * Data types
  , Module
  , Example
    -- * Interaction tests with GHCi.
  , Interpreter
  , withInterpreter
  , eval
  , runModules
    -- * Test result
  , Result (..)
  , isSucceeded
  ) where

import Interpreter
import Parse
import Report
