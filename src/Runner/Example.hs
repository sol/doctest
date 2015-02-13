module Runner.Example (
  Result (..)
, mkResult
) where

import           Data.Char
import           Util

import           Parse

data Result = Equal | NotEqual [String]
  deriving (Eq, Show)

mkResult :: ExpectedResult -> [String] -> Result
mkResult expected actual
  | expected `matches` actual = Equal
  | otherwise = NotEqual (formatNotEqual expected actual)
  where
    matches :: ExpectedResult -> [String] -> Bool
    matches [] [] = True
    matches [] _  = False
    matches _  [] = False
    matches (PlainResultLine x : xs) (y:ys) =
        stripEnd x == stripEnd y && xs `matches` ys
    matches zs@(WildCardLine : xs) (_:ys) =
        xs `matches` ys || zs `matches` ys


formatNotEqual :: ExpectedResult -> [String] -> [String]
formatNotEqual expected_ actual = formatLines "expected: " expected ++ formatLines " but got: " actual
  where
    expected :: [String]
    expected = map (\x -> case x of
        PlainResultLine str -> str
        WildCardLine -> "..." ) expected_

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
