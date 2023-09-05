module Util where

import           Imports

import           Data.Char

convertDosLineEndings :: String -> String
convertDosLineEndings = go
  where
    go input = case input of
      '\r':'\n':xs -> '\n' : go xs

      -- Haddock comments from source files with dos line endings end with a
      -- CR, so we strip that, too.
      "\r"         -> ""

      x:xs         -> x : go xs
      ""           -> ""

-- | Return the longest suffix of elements that satisfy a given predicate.
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

-- | Remove trailing white space from a string.
--
-- >>> stripEnd "foo   "
-- "foo"
stripEnd :: String -> String
stripEnd = reverse . dropWhile isSpace . reverse
