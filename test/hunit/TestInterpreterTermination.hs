{-# LANGUAGE TemplateHaskell #-}
module TestInterpreterTermination (tests) where

import Test.Framework.TH (testGroupGenerator)
import Test.HUnit
import Test.Framework.Providers.HUnit

import System.Process (readProcess)

tests = $(testGroupGenerator)

-- This test case triggers issue #3
-- see https://github.com/sol/doctest-haskell/issues/3
case_interpreter_termination = do
  s <- readProcess "TestInterpreterTermination/test_script.sh" [] ""
  "success\n" @=? s
