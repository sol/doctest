module Util where

import Data.List (stripPrefix)


-- |
-- Example:
--
-- ghci> replace "." "/" "Foo.Bar.Baz"
-- "Foo/Bar/Baz"
--
-- Source: Posted from Chaddaï Fouché to Haskell-Cafe mailing list.
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new xs@(y:ys) =
  case stripPrefix old xs of
    Nothing -> y : replace old new ys
    Just ys' -> new ++ replace old new ys'

-- | Strip given postfix from given list.
--
-- Examples:
--
-- ghci> stripPostfix "\n" "foobar\n"
-- "foobar"
--
-- ghci> stripPostfix "baz" "foobarbaz"
-- "foobar"
--
-- ghci> stripPostfix "\n" "foobar"
-- "foobar*** Exception: Prelude.undefined
--
stripPostfix :: (Eq a) => [a] -> [a] -> [a]
stripPostfix _ [] = undefined
stripPostfix postfix (x:xs)
  | xs == postfix = [x]
  | otherwise     = x : (stripPostfix postfix xs)
