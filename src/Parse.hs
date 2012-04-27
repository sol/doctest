module Parse (
  Example(..)
, Module (..)
, Interaction(..)
, getDocTests

-- * exported for testing
, parse
) where

import           Data.Char (isSpace)
import           Data.List
import           Data.Maybe (fromMaybe)

import           Extract
import           Location

data Example = Example [Located Interaction]
  deriving (Eq, Show)

type Expression = String
type ExpectedResult = [String]

data Interaction =
    Interaction Expression ExpectedResult
  | Property Expression
  deriving (Eq, Show)


-- |
-- Extract 'Example's from all given modules and all modules included by the
-- given modules.
getDocTests
  :: [String]             -- ^ List of GHC flags
  -> [String]             -- ^ File or module names
  -> IO [Module Example]  -- ^ Extracted 'Example's
getDocTests flags modules = do
  mods <- extract flags modules
  return (filter (not . null . moduleContent) $ map parseModule mods)

-- | Convert documentation to `Example`s.
parseModule :: Module (Located String) -> Module Example
parseModule (Module name docs) = (Module name . map Example . filter (not . null) . map parse) docs

-- | Extract all interactions from given Haddock documentation.
parse :: Located String -> [Located Interaction]
parse (Located loc input) = go $ zipWith Located (enumerate loc) (lines input)
  where
    isPrompt :: Located String -> Bool
    isPrompt x = ">>>" `isPrefixOf` sx || "prop>" `isPrefixOf` sx
       where
         sx = dropSpace (unLoc x)

    isInteraction :: Located String -> Bool
    isInteraction = isPrefixOf ">>>" . dropSpace . unLoc

    isBlankLine :: Located String -> Bool
    isBlankLine  = null . dropSpace . unLoc

    isEndOfInteraction :: Located String -> Bool
    isEndOfInteraction x = isPrompt x || isBlankLine x

    go :: [Located String] -> [Located Interaction]
    go xs =
      case dropWhile (not . isPrompt) xs of
        [] -> []
        prompt:rest
          | isInteraction prompt -> -- FIXME: doubly checking ">>>"
              let (ys,zs) = break isEndOfInteraction rest
              in toInteraction prompt ys : go zs
          | otherwise -> toProperty prompt : go rest

toProperty :: Located String -> Located Interaction
toProperty (Located loc x) =
    Located loc (Property . strip . drop 5 . dropSpace $ x)

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
strip = dropSpace . reverse . dropSpace . reverse

dropSpace :: String -> String
dropSpace = dropWhile isSpace
