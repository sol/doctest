-- |
-- An experimental API for extracting and executing `DocTest`s.
--
-- Use 'getDocTests' to extract 'DocTest' examples from Haddock comments.  To
-- verify that the examples work turn them into 'Test.HUnit.Assertion's, using
-- 'withInterpreter' and 'toAssertion'.  After this just wrap the newly minted
-- assertions into something suitable for your favorite test framework.
module Test.DocTest (
    module DocTest
  , withInterpreter
  , eval
  ) where

import DocTest
import Interpreter (withInterpreter, eval)
