module Runner.Example (
  Result (..)
, mkResult
) where

import           Data.Char
import           Data.List
import           Util

import           Parse

maxBy :: (Ord a) => (b -> a) -> b -> b -> b
maxBy f x y = case compare (f x) (f y) of
  LT -> y
  EQ -> x
  GT -> x

data Result = Equal | NotEqual [String]
  deriving (Eq, Show)

mkResult :: ExpectedResult -> [String] -> Result
mkResult expected_ actual_ =
  case expected `matches` actual of
  Full            -> Equal
  Partial partial -> NotEqual (formatNotEqual expected actual partial)
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

    chunksMatch :: [LineChunk] -> String -> Match ChunksDivergence
    chunksMatch [] "" = Full
    chunksMatch [LineChunk xs] ys =
      if stripEnd xs == stripEnd ys
      then Full
      else Partial $ matchingPrefix xs ys
    chunksMatch (LineChunk x : xs) ys =
      if x `isPrefixOf` ys
      then fmap (prependText x) $ (xs `chunksMatch` drop (length x) ys)
      else Partial $ matchingPrefix x ys
    chunksMatch zs@(WildCardChunk : xs) (_:ys) =
      -- Prefer longer matches.
      fmap prependWildcard $ maxBy
        (fmap $ length . matchText)
        (chunksMatch xs ys)
        (chunksMatch zs ys)
    chunksMatch [WildCardChunk] [] = Full
    chunksMatch (WildCardChunk:_) [] = Partial (ChunksDivergence "" "")
    chunksMatch [] (_:_) = Partial (ChunksDivergence "" "")

    matchingPrefix xs ys =
      let common = fmap fst (takeWhile (\(x, y) -> x == y) (xs `zip` ys)) in
      ChunksDivergence common common

    matches :: ExpectedResult -> [String] -> Match LinesDivergence
    matches (ExpectedLine x : xs) (y : ys) =
      case x `chunksMatch` y of
      Full -> fmap incLineNo $ xs `matches` ys
      Partial partial -> Partial (LinesDivergence 1 (expandedWildcards partial))
    matches zs@(WildCardLine : xs) us@(_ : ys) =
      -- Prefer longer matches, and later ones of equal length.
      let matchWithoutWC = xs `matches` us in
      let matchWithWC    = fmap incLineNo (zs `matches` ys) in
      let key (LinesDivergence lineNo line) = (length line, lineNo) in
      maxBy (fmap key) matchWithoutWC matchWithWC
    matches [WildCardLine] [] = Full
    matches [] [] = Full
    matches [] _  = Partial (LinesDivergence 1 "")
    matches _  [] = Partial (LinesDivergence 1 "")

-- Note: order of constructors matters, so that full matches sort as
-- greater than partial.
data Match a = Partial a | Full
  deriving (Eq, Ord, Show)

instance Functor Match where
  fmap f (Partial a) = Partial (f a)
  fmap _ Full = Full

data ChunksDivergence = ChunksDivergence { matchText :: String, expandedWildcards :: String }
  deriving (Show)

prependText :: String -> ChunksDivergence -> ChunksDivergence
prependText s (ChunksDivergence mt wct) = ChunksDivergence (s++mt) (s++wct)

prependWildcard :: ChunksDivergence -> ChunksDivergence
prependWildcard (ChunksDivergence mt wct) = ChunksDivergence mt ('.':wct)

data LinesDivergence = LinesDivergence { mismatchLineNo :: Int, partialLine :: String }
  deriving (Show)

incLineNo :: LinesDivergence -> LinesDivergence
incLineNo (LinesDivergence lineNo partialLineMatch) = LinesDivergence (lineNo + 1) partialLineMatch

formatNotEqual :: ExpectedResult -> [String] -> LinesDivergence -> [String]
formatNotEqual expected_ actual partial = formatLines "expected: " expected ++ formatLines " but got: " (lineMarker wildcard partial actual)
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

lineMarker :: Bool -> LinesDivergence -> [String] -> [String]
lineMarker wildcard (LinesDivergence row expanded) actual =
  let (pre, post) = splitAt row actual in
  pre ++
  [(if wildcard && length expanded > 30
    -- show expanded pattern if match is long, to help understanding what matched what
    then expanded
    else replicate (length expanded) ' ') ++ "^"] ++
  post
