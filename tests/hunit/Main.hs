module Main (main) where

import Test.Framework (defaultMain)

import qualified TestInterpreter

main = defaultMain [TestInterpreter.tests]
