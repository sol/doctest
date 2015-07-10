{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Parse (
  Module (..)
, DocTest (..)
, Interaction
, Expression
, ExpectedResult
, ExpectedLine (..)
, LineChunk (..)
, getDocTests

-- * exported for testing
, parseInteractions
, parseProperties
, mkLineChunks
) where

import           Data.Char (isSpace)
import           Data.List
import           Data.Maybe
import           Data.String
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Extract
import           Location


data DocTest = Example Expression ExpectedResult | Property Expression
  deriving (Eq, Show)

data LineChunk = LineChunk String | WildCardChunk
  deriving (Show, Eq)

instance IsString LineChunk where
    fromString = LineChunk

data ExpectedLine = ExpectedLine [LineChunk] | WildCardLine
  deriving (Show, Eq)

instance IsString ExpectedLine where
    fromString = ExpectedLine . return . LineChunk

type Expression = String
type ExpectedResult = [ExpectedLine]

type Interaction = (Expression, ExpectedResult)


-- |
-- Extract 'DocTest's from all given modules and all modules included by the
-- given modules.
getDocTests :: [String] -> IO [Module [Located DocTest]]  -- ^ Extracted 'DocTest's
getDocTests args = do
  filter (not . isEmpty) . map parseModule <$> extract args
  where
    isEmpty (Module _ setup tests) = null tests && isNothing setup

-- | Convert documentation to `Example`s.
parseModule :: Module (Located String) -> Module [Located DocTest]
parseModule m = case parseComment <$> m of
  Module name setup tests -> Module name setup_ (filter (not . null) tests)
    where
      setup_ = case setup of
        Just [] -> Nothing
        _       -> setup

parseComment :: Located String -> [Located DocTest]
parseComment c = properties ++ examples
  where
    examples   = map (fmap $ uncurry Example) (parseInteractions c)
    properties = map (fmap          Property) (parseProperties   c)

-- | Extract all properties from given Haddock comment.
parseProperties :: Located String -> [Located Expression]
parseProperties (Located loc input) = go $ zipWith Located (enumerate loc) (lines input)
  where
    isPrompt :: Located String -> Bool
    isPrompt = isPrefixOf "prop>" . dropWhile isSpace . unLoc

    go xs = case dropWhile (not . isPrompt) xs of
      prop:rest -> stripPrompt `fmap` prop : go rest
      [] -> []

    stripPrompt = strip . drop 5 . dropWhile isSpace

-- | Extract all interactions from given Haddock comment.
parseInteractions :: Located String -> [Located Interaction]
parseInteractions (Located loc input) = go $ zipWith Located (enumerate loc) (lines input)
  where
    isPrompt :: Located String -> Bool
    isPrompt = isPrefixOf ">>>" . dropWhile isSpace . unLoc

    isBlankLine :: Located String -> Bool
    isBlankLine  = null . dropWhile isSpace . unLoc

    isEndOfInteraction :: Located String -> Bool
    isEndOfInteraction x = isPrompt x || isBlankLine x


    go :: [Located String] -> [Located Interaction]
    go xs = case dropWhile (not . isPrompt) xs of
      prompt:rest
       | ":{" : _ <- words (drop 3 (dropWhile isSpace (unLoc prompt))),
         (ys,zs) <- break isBlankLine rest ->
          toInteraction prompt ys : go zs

       | otherwise ->
        let
          (ys,zs) = break isEndOfInteraction rest
        in
          toInteraction prompt ys : go zs
      [] -> []

-- | Create an `Interaction`, strip superfluous whitespace as appropriate.
--
-- also merge lines between :{ and :}, preserving whitespace inside
-- the block (since this is useful for avoiding {;}).
toInteraction :: Located String -> [Located String] -> Located Interaction
toInteraction (Located loc x) xs = Located loc $
  (
    (strip   cleanedE)  -- we do not care about leading and trailing
                        -- whitespace in expressions, so drop them
  , map mkExpectedLine result_
  )
  where
    -- 1. drop trailing whitespace from the prompt, remember the prefix
    (prefix, e) = span isSpace x
    (ePrompt, eRest) = splitAt 3 e

    -- 2. drop, if possible, the exact same sequence of whitespace
    -- characters from each result line
    unindent pre = map (tryStripPrefix pre . unLoc)

    cleanBody line = fromMaybe (unLoc line)
                    (stripPrefix ePrompt (dropWhile isSpace (unLoc line)))

    (cleanedE, result_)
            | (body , endLine : rest) <- break
                    ( (==) [":}"] . take 1 . words . cleanBody)
                    xs
                = (unlines (eRest : map cleanBody body ++
                                [dropWhile isSpace (cleanBody endLine)]),
                        unindent (takeWhile isSpace (unLoc endLine)) rest)
            | otherwise = (eRest, unindent prefix xs)


tryStripPrefix :: String -> String -> String
tryStripPrefix prefix ys = fromMaybe ys $ stripPrefix prefix ys

mkExpectedLine :: String -> ExpectedLine
mkExpectedLine x = case x of
    "<BLANKLINE>" -> ""
    "..." -> WildCardLine
    _ -> ExpectedLine $ mkLineChunks x

mkLineChunks :: String -> [LineChunk]
mkLineChunks = finish . foldr go (0, [], [])
  where
    mkChunk :: String -> [LineChunk]
    mkChunk "" = []
    mkChunk x  = [LineChunk x]

    go :: Char -> (Int, String, [LineChunk]) -> (Int, String, [LineChunk])
    go '.' (count, acc, res) = if count == 2
          then (0, "", WildCardChunk : mkChunk acc ++ res)
          else (count + 1, acc, res)
    go c   (count, acc, res) = if count > 0
          then (0, c : replicate count '.' ++ acc, res)
          else (0, c : acc, res)
    finish (count, acc, res) = mkChunk (replicate count '.' ++ acc) ++ res


-- | Remove leading and trailing whitespace.
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
