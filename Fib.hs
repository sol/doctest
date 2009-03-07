
module Fib where


{-
Example:

> fib 10
55
-}


-- Example:
--
-- > fib 10
-- 55


-- Example:
--
-- > fib 5
-- 5

-- Example:
--
-- > fib 5
-- 6

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
