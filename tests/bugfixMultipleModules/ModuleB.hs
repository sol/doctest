module ModuleB (fib) where


-- |
-- >>> fib 10
-- 55
-- >>> fib 5
-- 5
fib :: Integer -> Integer
fib = foo

-- |
-- >>> foo 10
-- 55
-- >>> foo 5
-- 5
foo :: Integer -> Integer
foo 0 = 0
foo 1 = 1
foo n = foo (n - 1) + foo (n - 2)
