{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module Options (
  Result(..)
, Run(..)
, defaultMagic
, defaultFastMode
, defaultPreserveIt
, defaultVerbose
, parseOptions
#ifdef TEST
, usage
, info
, versionInfo
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Trans.RWS (RWS, execRWS)
import qualified Control.Monad.Trans.RWS as RWS

import           Control.Monad (when)
import           Data.List.Compat
import           Data.Monoid (Endo (Endo))

import qualified Paths_doctest
import           Data.Version (showVersion)

#if __GLASGOW_HASKELL__ < 900
import           Config as GHC
#else
import           GHC.Settings.Config as GHC
#endif

import           Interpreter (ghc)

usage :: String
usage = unlines [
    "Usage:"
  , "  doctest [ --fast | --preserve-it | --no-magic | --verbose | GHC OPTION | MODULE ]..."
  , "  doctest --help"
  , "  doctest --version"
  , "  doctest --info"
  , ""
  , "Options:"
  , "  --fast         disable :reload between example groups"
  , "  --preserve-it  preserve the `it` variable between examples"
  , "  --verbose      print each test as it is run"
  , "  --help         display this help and exit"
  , "  --version      output version information and exit"
  , "  --info         output machine-readable version information and exit"
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
} deriving (Eq, Show)

defaultMagic :: Bool
defaultMagic = True

defaultFastMode :: Bool
defaultFastMode = False

defaultPreserveIt :: Bool
defaultPreserveIt = False

defaultVerbose :: Bool
defaultVerbose = False

defaultRun :: Run
defaultRun = Run {
  runWarnings = []
, runOptions = []
, runMagicMode = defaultMagic
, runFastMode = defaultFastMode
, runPreserveIt = defaultPreserveIt
, runVerbose = defaultVerbose
}

modifyWarnings :: ([String] -> [String]) -> Run -> Run
modifyWarnings f run = run { runWarnings = f (runWarnings run) }

setOptions :: [String] -> Run -> Run
setOptions opts run = run { runOptions = opts }

setMagicMode :: Bool -> Run -> Run
setMagicMode magic run = run { runMagicMode = magic }

setFastMode :: Bool -> Run -> Run
setFastMode fast run = run { runFastMode = fast }

setPreserveIt :: Bool -> Run -> Run
setPreserveIt preserveIt run = run { runPreserveIt = preserveIt }

setVerbose :: Bool -> Run -> Run
setVerbose verbose run = run { runVerbose = verbose }

parseOptions :: [String] -> Result Run
parseOptions args
  | "--help" `elem` args = Output usage
  | "--info" `elem` args = Output info
  | "--version" `elem` args = Output versionInfo
  | otherwise = case execRWS parse () args of
      (xs, Endo setter) ->
        Result (setOptions xs $ setter defaultRun)
    where
      parse :: RWS () (Endo Run) [String] ()
      parse = do
        stripNoMagic
        stripFast
        stripPreserveIt
        stripVerbose
        stripOptGhc

stripNoMagic :: RWS () (Endo Run) [String] ()
stripNoMagic = stripFlag (setMagicMode False) "--no-magic"

stripFast :: RWS () (Endo Run) [String] ()
stripFast = stripFlag (setFastMode True) "--fast"

stripPreserveIt :: RWS () (Endo Run) [String] ()
stripPreserveIt = stripFlag (setPreserveIt True) "--preserve-it"

stripVerbose :: RWS () (Endo Run) [String] ()
stripVerbose = stripFlag (setVerbose True) "--verbose"

stripFlag :: (Run -> Run) -> String -> RWS () (Endo Run) [String] ()
stripFlag setter flag = do
  args <- RWS.get
  when (flag `elem` args) $
    RWS.tell (Endo setter)
  RWS.put (filter (/= flag) args)

stripOptGhc :: RWS () (Endo Run) [String] ()
stripOptGhc = do
  issueWarning <- RWS.state go
  when issueWarning $
    RWS.tell $ Endo $ modifyWarnings (++ [warning])
  where
    go args = case args of
      [] -> (False, [])
      "--optghc" : opt : rest -> (True, opt : snd (go rest))
      opt : rest -> maybe (fmap (opt :)) (\x (_, xs) -> (True, x : xs)) (stripPrefix "--optghc=" opt) (go rest)

    warning = "WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."
