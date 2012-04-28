module Util where

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
