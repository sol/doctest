module Foo.ModuleB (fib) where


-- |
-- >>> fib 10
-- 55
-- >>> fib 5
-- 5
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
