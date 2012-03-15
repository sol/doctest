-- | module header
module Foo (

-- * some heading
-- | export list 1
  foo

-- * some other heading
-- | export list 2
, bar

-- * one more heading
-- $foo
, baz
) where

-- | foo
foo :: Int
foo = 23

-- $foo named chunk

-- | bar
bar :: Int
bar = 23

baz :: Int
baz = 23
