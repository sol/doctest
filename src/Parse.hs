module Parse (
  DocTest(..)
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

data DocTest = DocExample [Interaction]
  deriving (Eq, Show)


data Interaction = Interaction {
  expression :: String    -- ^ example expression
, result     :: [String]  -- ^ expected result
} deriving (Eq, Show)


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

-- | Convert documentation to `DocTest`s.
parseModule :: Module String -> Module DocTest
parseModule (Module name docs) = (Module name . map DocExample . filter (not . null) . map parse) docs

-- | Extract all interactions from given Haddock documentation.
parse :: String -> [Interaction]
parse input = go (lines input)
  where
    isPrompt = isPrefixOf ">>>" . dropWhile isSpace
    isBlankLine  = null . dropWhile isSpace
    isEndOfInteraction x = isPrompt x || isBlankLine x

    go :: [String] -> [Interaction]
    go xs =
      case dropWhile (not . isPrompt) xs of
        prompt:rest ->
          let 
            (ys,zs) = break isEndOfInteraction rest
          in
            toInteraction prompt ys : go zs
        _ -> []

-- | Create an `Interaction`, strip superfluous whitespace as appropriate.
toInteraction :: String -> [String] -> Interaction
toInteraction x xs =
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
    result_ = map (substituteBlankLine . tryStripPrefix prefix) xs
      where
        tryStripPrefix pre ys = fromMaybe ys $ stripPrefix pre ys

        substituteBlankLine "<BLANKLINE>" = ""
        substituteBlankLine line          = line

-- | Remove leading and trailing whitespace.
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
