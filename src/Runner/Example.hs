module Runner.Example (
  Result (..)
, mkResult
) where

import           Data.Char
import           Util

data Result = Equal | NotEqual [String]
  deriving (Eq, Show)

mkResult :: [String] -> [String] -> Result
mkResult expected_ actual_
  | expected == actual = Equal
  | otherwise = NotEqual (formatNotEqual expected actual)
  where
    expected = map stripEnd expected_
    actual   = map stripEnd actual_

formatNotEqual :: [String] -> [String] -> [String]
formatNotEqual expected actual = formatLines "expected: " expected ++ formatLines " but got: " actual
  where
    -- use show to escape special characters in output lines if any output line
    -- contains any unsafe character
    escapeOutput
      | any (not . isSafe) (concat $ expected ++ actual) = map show
      | otherwise = id

    isSafe :: Char -> Bool
    isSafe c = c == ' ' || (isPrint c && (not . isSpace) c)

    formatLines :: String -> [String] -> [String]
    formatLines message xs = case escapeOutput xs of
      y:ys -> (message ++ y) : map (padding ++) ys
      []   -> [message]
      where
        padding = replicate (length message) ' '
