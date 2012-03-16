{-# LANGUAGE ParallelListComp #-}
module ParallelListComp where

foo :: [Int]
foo = [x+y | x <- [1,2,3] | y <- [4,5,6]]
