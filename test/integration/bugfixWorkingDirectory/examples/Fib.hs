module Fib where


-- | Calculate Fibonacci number of given 'Num'.
--
-- Examples:
--
--    >>> fib 10
--    55
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- |
--
-- Examples:
--
--    >>> fib 10
--    55
foo :: Int -> Int
foo = undefined
