module Fib where

-- | Calculate Fibonacci number of given 'Num'.
--
-- First let's set `n` to ten:
--
-- >>> let n = 10
--
-- And now calculate the 10th Fibonacci number:
--
-- >>> fib n
-- 55
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
