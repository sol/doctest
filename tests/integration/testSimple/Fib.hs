module Fib where

-- | Calculate Fibonacci numbers.
--
-- >>> fib 10
-- 55
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
