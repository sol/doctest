{-# LANGUAGE ParallelListComp #-}
module ParallelListCompClass where

class Foo a where
  foo :: a -> [Int]

instance Foo Int where
  foo _ = [x+y | x <- [1,2,3] | y <- [4,5,6]]
