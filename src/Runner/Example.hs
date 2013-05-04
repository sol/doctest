module Runner.Example (
  Result (..)
, mkResult
) where

import           Data.Char

data Result = Equal | NotEqual [String]
  deriving (Eq, Show)

mkResult :: [String] -> [String] -> Result
mkResult expected actual
  | expected == actual = Equal
  | otherwise = NotEqual (formatNotEqual expected actual)
  where

formatNotEqual :: [String] -> [String] -> [String]
formatNotEqual expected actual = formatLines "expected: " expected ++ formatLines " but got: " actual
  where
    -- print quotes if any line ends with trailing whitespace
    printQuotes = any isSpace (map last . filter (not . null) $ expected ++ actual)

    -- use show to escape special characters in output lines if any output line
    -- contains any unsafe character
    escapeOutput = any (not . isSafe) (concat $ expected ++ actual)

    isSafe :: Char -> Bool
    isSafe c = c == ' ' || (isPrint c && (not . isSpace) c)

    formatLines :: String -> [String] -> [String]
    formatLines message l_ = case l of
      x:xs -> (message ++ x) : map (padding ++) xs
      []   -> [message]
      where
        l | printQuotes || escapeOutput = map show l_
          | otherwise                   = l_

        padding = replicate (length message) ' '
