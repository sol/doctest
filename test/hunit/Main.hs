module Main (main) where

import Test.Framework (defaultMain)

import qualified TestInterpreterTermination

main = defaultMain [TestInterpreterTermination.tests]
