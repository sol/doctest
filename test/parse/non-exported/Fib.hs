module Fib (foo) where

foo :: Int
foo = 23

-- | Calculate Fibonacci number of given 'Num'.
--
-- >>> putStrLn "foo"
-- foo
-- >>> putStr "bar"
-- bar
--
-- >>> putStrLn "baz"
-- baz
fib :: (Num t, Num t1) => t -> t1
fib _ = undefined
