module Fibtest where
import Test.HUnit
import Fib

docTest = TestCase (assertEqual "Fib.hs (line 6): " (show (fib 10)) "55")
