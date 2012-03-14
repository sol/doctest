module Main (main) where

import Test.Framework (defaultMain)

import qualified TestInterpreter
import qualified TestInterpreterTermination

main = defaultMain [TestInterpreter.tests, TestInterpreterTermination.tests]
