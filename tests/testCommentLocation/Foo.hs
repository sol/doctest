-- |
-- Examples in various locations...
--
-- Some random text.  Some random text.  Some random text.  Some random text.
-- Some random text.  Some random text.  Some random text.  Some random text.
-- Some random text.
--
-- >>> let x = 10
--
-- Some random text.  Some random text.  Some random text.  Some random text.
-- Some random text.  Some random text.  Some random text.  Some random text.
-- Some random text.
--
--
--   >>> baz
--   "foobar"

module Foo (
  -- | Some documentation not attached to a particular Haskell entity
  --
  -- >>> test 10
  -- *** Exception: Prelude.undefined
  test,

  -- |
  -- >>> fib 10
  -- 55
  fib,

  -- |
  -- >>> bar
  -- "bar"
  bar
  ) where


-- | My test
--
-- >>> test 20
-- *** Exception: Prelude.undefined
test :: Integer -> Integer
test = undefined

-- |
-- >>> foo
-- "foo"

{- |
    Example:

     >>> fib 10
     55
-}

-- | Calculate Fibonacci number of given `n`.
fib :: Integer  -- ^ given `n`
                --
                -- >>> fib 10
                -- 55

    -> Integer  -- ^ Fibonacci of given `n`
                --
                -- >>> baz
                -- "foobar"
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
-- ^ Example:
--
--   >>> fib 5
--   5

foo = "foo"
bar = "bar"
baz = foo ++ bar
