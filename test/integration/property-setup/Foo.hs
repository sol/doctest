module Foo where

-- $setup
-- >>> import Test.QuickCheck
-- >>> let arbitraryEven = (* 2) `fmap` arbitrary

-- |
-- prop> forAll arbitraryEven even
foo = undefined
