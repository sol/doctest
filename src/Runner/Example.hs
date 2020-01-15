module Runner.Example (
  Result (..)
, mkResult
) where

import           Data.Char
import           Data.List
import           Util

import           Parse

data Result = Equal | NotEqual [String]
  deriving (Eq, Show)

mkResult :: ExpectedResult -> [String] -> Result
mkResult expected_ actual_ =
  case expected `matches` actual of
  FullMatch                 -> Equal
  PartialMatch row expanded -> NotEqual (formatNotEqual expected actual expanded row)
  where
    -- use show to escape special characters in output lines if any output line
    -- contains any unsafe character
    escapeOutput
      | any (not . isSafe) $ concat (expectedAsString ++ actual_) = init . tail . show . stripEnd
      | otherwise = id

    actual :: [String]
    actual = fmap escapeOutput actual_

    expected :: ExpectedResult
    expected = fmap (transformExcpectedLine escapeOutput) expected_

    expectedAsString :: [String]
    expectedAsString = map (\x -> case x of
        ExpectedLine str -> concatMap lineChunkToString str
        WildCardLine -> "..." ) expected_

    isSafe :: Char -> Bool
    isSafe c = c == ' ' || (isPrint c && (not . isSpace) c)

    chunksMatch :: [LineChunk] -> String -> LineMatch
    chunksMatch [] "" = FullLineMatch
    chunksMatch [LineChunk xs] ys =
      if stripEnd xs == stripEnd ys
      then FullLineMatch
      else matchingPrefix xs ys
    chunksMatch (LineChunk x : xs) ys =
      if x `isPrefixOf` ys
      then (xs `chunksMatch` drop (length x) ys) `prependText` x
      else matchingPrefix x ys
    chunksMatch zs@(WildCardChunk : xs) (_:ys) =
      let resWithoutWC = xs `chunksMatch` ys in
      let resWithWC = zs `chunksMatch` ys in
      let res = longerMatch resWithoutWC resWithWC in
      prependWildcard res
    chunksMatch [WildCardChunk] [] = FullLineMatch
    chunksMatch (WildCardChunk:_) [] = PartialLineMatch "" ""
    chunksMatch [] (_:_) = PartialLineMatch "" ""

    matchingPrefix xs ys =
      let common = fmap fst (takeWhile (\(x, y) -> x == y) (xs `zip` ys)) in
      PartialLineMatch common common

    matches :: ExpectedResult -> [String] -> Match
    matches (ExpectedLine x : xs) (y : ys) =
      case x `chunksMatch` y of
      FullLineMatch -> incLineNo $ xs `matches` ys
      PartialLineMatch _ expanded -> PartialMatch 1 expanded
    matches zs@(WildCardLine : xs) us@(_ : ys) =
      let matchWithoutWC = xs `matches` us in
      let matchWithWC    = zs `matches` ys in
      matchWithoutWC `matchMax` (incLineNo matchWithWC)
      where
        matchMax FullMatch _ = FullMatch
        matchMax _ FullMatch = FullMatch
        matchMax m1 m2 =
          if length (partialLine m1) > length (partialLine m2)
          then m1
          else if length (partialLine m2) > length (partialLine m2)
          then m2
          else if mismatchLineNo m1 > mismatchLineNo m2
          then m1
          else m2
    matches [WildCardLine] [] = FullMatch
    matches [] [] = FullMatch
    matches [] _  = PartialMatch 1 ""
    matches _  [] = PartialMatch 1 ""

data LineMatch = FullLineMatch | PartialLineMatch { matchingText :: String, _expandedWildcards :: String }
  deriving (Show)

prependText :: LineMatch -> String -> LineMatch
prependText FullLineMatch _ = FullLineMatch
prependText (PartialLineMatch mt wct) s = PartialLineMatch (s++mt) (s++wct)

prependWildcard :: LineMatch -> LineMatch
prependWildcard FullLineMatch = FullLineMatch
prependWildcard (PartialLineMatch mt wct) = PartialLineMatch mt ('.':wct)

longerMatch :: LineMatch -> LineMatch -> LineMatch
longerMatch FullLineMatch _ = FullLineMatch
longerMatch _ FullLineMatch = FullLineMatch
longerMatch m1 m2 =
  if length (matchingText m1) > length (matchingText m2) then m1 else m2

data Match = FullMatch | PartialMatch { mismatchLineNo :: Int, partialLine :: String }
  deriving (Show)

incLineNo :: Match -> Match
incLineNo FullMatch = FullMatch
incLineNo (PartialMatch lineNo partialLineMatch) = PartialMatch (lineNo + 1) partialLineMatch

formatNotEqual :: ExpectedResult -> [String] -> String -> Int -> [String]
formatNotEqual expected_ actual expanded row = formatLines "expected: " expected ++ formatLines " but got: " (lineMarker wildcard expanded row actual)
  where
    expected :: [String]
    expected = map (\x -> case x of
        ExpectedLine str -> concatMap lineChunkToString str
        WildCardLine -> "..." ) expected_

    formatLines :: String -> [String] -> [String]
    formatLines message xs = case xs of
      y:ys -> (message ++ y) : map (padding ++) ys
      []   -> [message]
      where
        padding = replicate (length message) ' '

    wildcard :: Bool
    wildcard = any (\x -> case x of
        ExpectedLine xs -> any (\y -> case y of { WildCardChunk -> True; _ -> False }) xs
        WildCardLine -> True ) expected_

lineChunkToString :: LineChunk -> String
lineChunkToString WildCardChunk = "..."
lineChunkToString (LineChunk str) = str

transformExcpectedLine :: (String -> String) -> ExpectedLine -> ExpectedLine
transformExcpectedLine f (ExpectedLine xs) =
  ExpectedLine $ fmap (\el -> case el of
    LineChunk s -> LineChunk $ f s
    WildCardChunk -> WildCardChunk
  ) xs
transformExcpectedLine _ WildCardLine = WildCardLine

lineMarker :: Bool -> String -> Int -> [String] -> [String]
lineMarker wildcard expanded row actual =
  let (pre, post) = splitAt row actual in
  pre ++
  [(if wildcard && length expanded > 30
    -- show expanded pattern if match is long, to help understanding what matched what
    then expanded
    else replicate (length expanded) ' ') ++ "^"] ++
  post
