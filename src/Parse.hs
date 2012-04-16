module Parse (
  Example(..)
, exampleLabel
, Module (..) -- re-exporting
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

-- |
-- Abstract data type for examples in Haddock.
-- All example interactions in a comment of a function are stored.
data Example = Example [Located Interaction]
  deriving (Eq, Show)

-- | Extracting 'String' which is suitable for labeling
exampleLabel :: Example -> String
exampleLabel (Example [])    = ""
exampleLabel (Example ((Located _ e):_)) = expression e

-- |
-- Concrete values in 'Example'.
data Interaction = Interaction {
  expression :: String    -- ^ example expression
, result     :: [String]  -- ^ expected result
} deriving (Eq, Show)


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
parse :: (Located String) -> [Located Interaction]
parse (Located loc input) = go $ zipWith Located (enumerate loc) (lines input)
  where
    isPrompt :: Located String -> Bool
    isPrompt = isPrefixOf ">>>" . dropWhile isSpace . unLoc

    isBlankLine :: Located String -> Bool
    isBlankLine  = null . dropWhile isSpace . unLoc

    isEndOfInteraction :: Located String -> Bool
    isEndOfInteraction x = isPrompt x || isBlankLine x

    go :: [Located String] -> [Located Interaction]
    go xs =
      case dropWhile (not . isPrompt) xs of
        prompt:rest ->
          let 
            (ys,zs) = break isEndOfInteraction rest
          in
            toInteraction prompt ys : go zs
        _ -> []

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
    result_ = map (substituteBlankLine . tryStripPrefix prefix) (map unLoc xs)
      where
        tryStripPrefix pre ys = fromMaybe ys $ stripPrefix pre ys

        substituteBlankLine "<BLANKLINE>" = ""
        substituteBlankLine line          = line

-- | Remove leading and trailing whitespace.
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
