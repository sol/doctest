{-# LANGUAGE TemplateHaskell #-}
module Foo where

import Bar

-- | some documentation
foo :: Int
foo = $(bar)
