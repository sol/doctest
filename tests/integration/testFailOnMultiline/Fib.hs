module Fib where

import System

-- | Calculate Fibonacci number of given 'Num'.
--
-- The following interactions cause `doctest' to fail with an error:
--
-- >>> :{
--
-- >>>       :{
fib :: (Num t, Num t1) => t -> t1
fib _ = undefined
