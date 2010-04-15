-- |
-- Examples in various locations...
--
-- Some random text.  Some random text.  Some random text.  Some random text.
-- Some random text.  Some random text.  Some random text.  Some random text.
-- Some random text.
--
-- ghci> let x = 10
--
-- Some random text.  Some random text.  Some random text.  Some random text.
-- Some random text.  Some random text.  Some random text.  Some random text.
-- Some random text.
--
--
--   ghci> baz
--   "foobar"

module Foo (
  -- | Some documentation not attached to a particular Haskell entity
  --
  -- ghci> test 10
  -- *** Exception: Prelude.undefined
  test,

  -- |
  -- ghci> fib 10
  -- 55
  fib,

  -- |
  -- ghci> bar
  -- "bar"
  bar
  ) where


-- | My test
--
-- ghci> test 20
-- *** Exception: Prelude.undefined
test :: Integer -> Integer
test = undefined

-- |
-- ghci> foo
-- "foo"

{- |
    Example:

     ghci> fib 10
     55
-}

-- | Calculate Fibonacci number of given `n`.
fib :: Integer  -- ^ given `n`
                --
                -- ghci> fib 10
                -- 55

    -> Integer  -- ^ Fibonacci of given `n`
                --
                -- ghci> baz
                -- "foobar"
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
-- ^ Example:
--
--   ghci> fib 5
--   5

foo = "foo"
bar = "bar"
baz = foo ++ bar
