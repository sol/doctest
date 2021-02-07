{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module Options (
  Result(..)
, Run(..)
, defaultMagic
, defaultFastMode
, defaultPreserveIt
, defaultVerbose
, defaultIsolateModules
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
  , "  doctest [ --fast | --preserve-it | --no-magic | --verbose | --isolate-modules | GHC OPTION | MODULE ]..."
  , "  doctest --help"
  , "  doctest --version"
  , "  doctest --info"
  , ""
  , "Options:"
  , "  --fast            disable :reload between example groups"
  , "  --preserve-it     preserve the `it` variable between examples"
  , "  --verbose         print each test as it is run"
  , "  --isolate-modules use new ghci process for each module"
  , "  --help            display this help and exit"
  , "  --version         output version information and exit"
  , "  --info            output machine-readable version information and exit"
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

data Result a = Output String | Result a
  deriving (Eq, Show, Functor)

type Warning = String

data Run = Run {
  runWarnings :: [Warning]
, runOptions :: [String]
, runMagicMode :: Bool
, runFastMode :: Bool
, runPreserveIt :: Bool
, runVerbose :: Bool
, runIsolateModules :: Bool
} deriving (Eq, Show)

defaultIsolateModules :: Bool
defaultIsolateModules = False

defaultMagic :: Bool
defaultMagic = True

defaultFastMode :: Bool
defaultFastMode = False

defaultPreserveIt :: Bool
defaultPreserveIt = False

defaultVerbose :: Bool
defaultVerbose = False

parseOptions :: [String] -> Result Run
parseOptions args
  | "--help" `elem` args = Output usage
  | "--info" `elem` args = Output info
  | "--version" `elem` args = Output versionInfo
  | otherwise = case  fmap (fmap (fmap (fmap stripOptGhc)))
                   .  fmap (fmap (fmap stripIsolateModules))
                   .  fmap (fmap stripVerbose)
                   .  fmap stripPreserveIt
                   .  stripFast
                  <$> stripNoMagic args of
      (magicMode, (fastMode, (preserveIt, (verbose, (isolate, (warning, xs)))))) ->
        Result (Run (maybeToList warning) xs magicMode fastMode preserveIt verbose isolate)

stripIsolateModules :: [String] -> (Bool, [String])
stripIsolateModules = stripFlag (not defaultIsolateModules) "--isolate-modules"

stripNoMagic :: [String] -> (Bool, [String])
stripNoMagic = stripFlag (not defaultMagic) "--no-magic"

stripFast :: [String] -> (Bool, [String])
stripFast = stripFlag (not defaultFastMode) "--fast"

stripPreserveIt :: [String] -> (Bool, [String])
stripPreserveIt = stripFlag (not defaultPreserveIt) "--preserve-it"

stripVerbose :: [String] -> (Bool, [String])
stripVerbose = stripFlag (not defaultVerbose) "--verbose"

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
