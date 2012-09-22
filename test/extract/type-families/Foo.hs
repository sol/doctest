{-# LANGUAGE TypeFamilies #-}
module Foo where

type family Foo a

type instance Foo Int = Int
