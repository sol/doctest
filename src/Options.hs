{-# LANGUAGE CPP #-}
module Options (
  Result(..)
, Run(..)
, parseOptions
#ifdef TEST
, usage
, info
, versionInfo
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Data.List.Compat
import           Data.Maybe

import qualified Paths_doctest
import           Data.Version (showVersion)
import           Config as GHC
import           Interpreter (ghc)

usage :: String
usage = unlines [
    "Usage:"
  , "  doctest [ --fast | --no-magic | GHC OPTION | MODULE ]..."
  , "  doctest --help"
  , "  doctest --version"
  , "  doctest --info"
  , ""
  , "Options:"
  , "  --help     display this help and exit"
  , "  --version  output version information and exit"
  , "  --info     output machine-readable version information and exit"
  ]

version :: String
version = showVersion Paths_doctest.version

ghcVersion :: String
ghcVersion = GHC.cProjectVersion

versionInfo :: String
versionInfo = unlines [
    "doctest version " ++ version
  , "using version " ++ ghcVersion ++ " of the GHC API"
  , "using " ++ ghc
  ]

info :: String
info = "[ " ++ (intercalate "\n, " . map show $ [
    ("version", version)
  , ("ghc_version", ghcVersion)
  , ("ghc", ghc)
  ]) ++ "\n]\n"

data Result = Output String | Result Run
  deriving (Eq, Show)

type Warning = String

data Run = Run {
  runWarnings :: [Warning]
, runOptions :: [String]
, runMagicMode :: Bool
, runFastMode :: Bool
} deriving (Eq, Show)

parseOptions :: [String] -> Result
parseOptions args
  | "--help" `elem` args = Output usage
  | "--info" `elem` args = Output info
  | "--version" `elem` args = Output versionInfo
  | otherwise = case fmap stripOptGhc . stripFast <$> stripNoMagic args of
      (magicMode, (fastMode, (warning, xs))) ->
        Result (Run (maybeToList warning) xs magicMode fastMode)

stripNoMagic, stripFast :: [String] -> (Bool, [String])
stripNoMagic = stripFlag False "--no-magic"
stripFast    = stripFlag True  "--fast"

stripFlag :: Bool -> String -> [String] -> (Bool, [String])
stripFlag enableIt flag args = ((flag `elem` args) == enableIt, filter (/= flag) args)

stripOptGhc :: [String] -> (Maybe Warning, [String])
stripOptGhc = go
  where
    go args = case args of
      [] -> (Nothing, [])
      "--optghc" : opt : rest -> (Just warning, opt : snd (go rest))
      opt : rest -> maybe (fmap (opt :)) (\x (_, xs) -> (Just warning, x : xs)) (stripPrefix "--optghc=" opt) (go rest)

    warning = "WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."
