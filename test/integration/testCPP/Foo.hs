module Foo where


-- |
-- Examples:
--
-- >>> foo
-- 23
foo :: Int
#ifdef FOO
foo = 23
#else
foo = 42
#endif
