module ModuleB (fib) where


-- |
-- ghci> fib 10
-- 55
-- ghci> fib 5
-- 5
fib :: Integer -> Integer
fib = foo

-- |
-- ghci> foo 10
-- 55
-- ghci> foo 5
-- 5
foo :: Integer -> Integer
foo 0 = 0
foo 1 = 1
foo n = foo (n - 1) + foo (n - 2)
