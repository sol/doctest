module Runner.Example (
  Result (..)
, mkResult
) where

import           Data.Char
import           Data.List
import           Util
import           Text.Regex.TDFA (Regex, makeRegexM, matchTest)

import           Parse

data Result = Equal | NotEqual [String]
  deriving (Eq, Show)

mkResult :: ExpectedResult -> [String] -> Result
mkResult expected actual
  | expected `matches` actual = Equal
  | otherwise = NotEqual (formatNotEqual expected actual)
  where
    chunksMatch :: [LineChunk] -> String -> Bool
    chunksMatch [] "" = True
    chunksMatch [LineChunk xs] ys = stripEnd xs == stripEnd ys
    chunksMatch (LineChunk x : xs) ys =
        x `isPrefixOf` ys && xs `chunksMatch` drop (length x) ys
    chunksMatch zs@(WildCardChunk : xs) (_:ys) =
        xs `chunksMatch` ys || zs `chunksMatch` ys
    chunksMatch _ _ = False

    reMatch :: String -> String -> Bool
    reMatch restr line = case re of
        Nothing -> False
        Just re' -> matchTest re' line
      where
        re :: Maybe Regex
        re = makeRegexM restr

    matches :: ExpectedResult -> [String] -> Bool
    matches (ExpectedLine x : xs) (y : ys) = x `chunksMatch` y && xs `matches` ys
    matches (RegexLine re : xs) (y:ys) = re `reMatch` y && xs `matches` ys
    matches (WildCardLine : xs) ys | xs `matches` ys = True
    matches zs@(WildCardLine : _) (_ : ys) = zs `matches` ys
    matches [] [] = True
    matches [] _  = False
    matches _  [] = False


formatNotEqual :: ExpectedResult -> [String] -> [String]
formatNotEqual expected_ actual = formatLines "expected: " expected ++ formatLines " but got: " actual
  where
    expected :: [String]
    expected = map (\x -> case x of
        ExpectedLine str -> concatMap lineChunkToString str
        RegexLine re -> "~~~" ++ re
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

lineChunkToString :: LineChunk -> String
lineChunkToString WildCardChunk = "..."
lineChunkToString (LineChunk str) = str
