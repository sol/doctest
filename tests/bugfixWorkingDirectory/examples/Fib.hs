module Fib where


-- | Calculate Fibonacci number of given 'Num'.
--
-- Examples:
--
--    >>> fib 10
--    55
fib :: (Num t, Num t1) => t -> t1
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
