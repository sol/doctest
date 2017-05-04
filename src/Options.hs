{-# LANGUAGE CPP #-}
module Options (
  Result(..)
, Run(..)
, parseOptions
#ifdef TEST
, usage
, versionInfo
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Data.List.Compat
import           Data.Maybe

import           Paths_doctest (version)
import           Data.Version (showVersion)
import           Config as GHC
import           Interpreter (ghc)

usage :: String
usage = unlines [
    "Usage:"
  , "  doctest [ --no-magic | GHC OPTION | MODULE ]..."
  , "  doctest --help"
  , "  doctest --version"
  , ""
  , "Options:"
  , "  --help     display this help and exit"
  , "  --version  output version information and exit"
  ]

versionInfo :: String
versionInfo = unlines [
    "doctest version " ++ showVersion version
  , "using version " ++ GHC.cProjectVersion ++ " of the GHC API"
  , "using " ++ ghc
  ]

data Result = Output String | Result Run
  deriving (Eq, Show)

type Warning = String

data Run = Run {
  runWarnings :: [Warning]
, runOptions :: [String]
, runMagicMode :: Bool
} deriving (Eq, Show)

parseOptions :: [String] -> Result
parseOptions args
  | "--help" `elem` args = Output usage
  | "--version" `elem` args = Output versionInfo
  | otherwise = case stripOptGhc <$> stripNoMagic args of
      (magicMode, (warning, xs)) -> Result (Run (maybeToList warning) xs magicMode)

stripNoMagic :: [String] -> (Bool, [String])
stripNoMagic args = (noMagic `notElem` args, filter (/= noMagic) args)
  where
    noMagic = "--no-magic"

stripOptGhc :: [String] -> (Maybe Warning, [String])
stripOptGhc = go
  where
    go args = case args of
      [] -> (Nothing, [])
      "--optghc" : opt : rest -> (Just warning, opt : snd (go rest))
      opt : rest -> maybe (fmap (opt :)) (\x (_, xs) -> (Just warning, x : xs)) (stripPrefix "--optghc=" opt) (go rest)

    warning = "WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."
