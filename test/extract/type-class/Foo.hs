module Foo where


class ToString a where

  -- | Convert given value to a string.
  toString :: a -> String
