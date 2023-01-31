{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module Options (
  Result(..)
, Run(..)
, parseOptions
#ifdef TEST
, defaultRun
, usage
, info
, versionInfo
, nonInteractiveGhcOptions
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Trans.RWS (RWS, execRWS)
import qualified Control.Monad.Trans.RWS as RWS

import           Control.Monad (when)
import           Data.List.Compat (stripPrefix)
import           Data.Monoid (Endo (Endo))

import           Info

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

data Result a = RunGhc [String] | Output String | Result a
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

nonInteractiveGhcOptions :: [String]
nonInteractiveGhcOptions = [
    "--numeric-version"
  , "--supported-languages"
  , "--info"
  , "--print-global-package-db"
  , "--print-libdir"
  , "-c"
  , "-o"
  , "--make"
  , "--abi-hash"
  ]

defaultRun :: Run
defaultRun = Run {
  runWarnings = []
, runOptions = []
, runMagicMode = False
, runFastMode = False
, runPreserveIt = False
, runVerbose = False
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
  | on "--info" = Output info
  | on "--interactive" = runRunOptionsParser (discard "--interactive" args) defaultRun $ do
      commonRunOptions
  | on `any` nonInteractiveGhcOptions = RunGhc args
  | on "--help" = Output usage
  | on "--version" = Output versionInfo
  | otherwise = runRunOptionsParser args defaultRun {runMagicMode = True} $ do
      commonRunOptions
      parseFlag "--no-magic" (setMagicMode False)
      parseOptGhc
  where
    on option = option `elem` args

type RunOptionsParser = RWS () (Endo Run) [String] ()

runRunOptionsParser :: [String] -> Run -> RunOptionsParser -> Result Run
runRunOptionsParser args def parse = case execRWS parse () args of
  (xs, Endo setter) ->
    Result (setOptions xs $ setter def)

commonRunOptions :: RunOptionsParser
commonRunOptions = do
  parseFlag "--fast" (setFastMode True)
  parseFlag "--preserve-it" (setPreserveIt True)
  parseFlag "--verbose" (setVerbose True)

parseFlag :: String -> (Run -> Run) -> RunOptionsParser
parseFlag flag setter = do
  args <- RWS.get
  when (flag `elem` args) $
    RWS.tell (Endo setter)
  RWS.put (discard flag args)

parseOptGhc :: RunOptionsParser
parseOptGhc = do
  issueWarning <- RWS.state go
  when issueWarning $
    RWS.tell $ Endo $ modifyWarnings (++ [warning])
  where
    go args = case args of
      [] -> (False, [])
      "--optghc" : opt : rest -> (True, opt : snd (go rest))
      opt : rest -> maybe (fmap (opt :)) (\x (_, xs) -> (True, x : xs)) (stripPrefix "--optghc=" opt) (go rest)

    warning = "WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."

discard :: String -> [String] -> [String]
discard flag = filter (/= flag)
