module Parse (
  Module (..)
, DocTest (..)
, Interaction (..)
, Expression
, ExpectedResult
, getDocTests

-- * exported for testing
, parseInteractions
, parseProperties
) where

import           Data.Char (isSpace)
import           Data.List
import           Data.Maybe (fromMaybe)

import           Extract
import           Location

data DocTest = Example [Located Interaction] | Property (Located Expression)
  deriving (Eq, Show)

type Expression = String
type ExpectedResult = [String]

data Interaction = Interaction Expression ExpectedResult
  deriving (Eq, Show)

-- |
-- Extract 'DocTest's from all given modules and all modules included by the
-- given modules.
getDocTests
  :: [String]             -- ^ List of GHC flags
  -> [String]             -- ^ File or module names
  -> IO [Module DocTest]  -- ^ Extracted 'DocTest's
getDocTests flags modules = do
  mods <- extract flags modules
  return (filter (not . null . moduleContent) $ map parseModule mods)

-- | Convert documentation to `Example`s.
parseModule :: Module (Located String) -> Module DocTest
parseModule (Module name docs) = Module name (properties ++ examples)
  where
    examples = (map Example . filter (not . null) . map parseInteractions) docs
    properties = (map Property . concat . map parseProperties) docs

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
      prompt:rest ->
        let
          (ys,zs) = break isEndOfInteraction rest
        in
          toInteraction prompt ys : go zs
      [] -> []

-- | Create an `Interaction`, strip superfluous whitespace as appropriate.
toInteraction :: Located String -> [Located String] -> Located Interaction
toInteraction (Located loc x) xs = Located loc $
  Interaction
    (strip $ drop 3 e)  -- we do not care about leading and trailing
                        -- whitespace in expressions, so drop them
    result_
  where
    -- 1. drop trailing whitespace from the prompt, remember the prefix
    (prefix, e) = span isSpace x

    -- 2. drop, if possible, the exact same sequence of whitespace
    -- characters from each result line
    --
    -- 3. interpret lines that only contain the string "<BLANKLINE>" as an
    -- empty line
    result_ = map (substituteBlankLine . tryStripPrefix prefix . unLoc) xs
      where
        tryStripPrefix pre ys = fromMaybe ys $ stripPrefix pre ys

        substituteBlankLine "<BLANKLINE>" = ""
        substituteBlankLine line          = line

-- | Remove leading and trailing whitespace.
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
