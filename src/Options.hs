{-# LANGUAGE CPP #-}
module Options (
  Result(..)
, Run(..)
, Config(..)
, defaultConfig
, parseOptions
#ifdef TEST
, defaultRun
, usage
, info
, versionInfo
, nonInteractiveGhcOptions
#endif
) where

import           Imports

import           Control.Monad.Trans.RWS (RWS, execRWS)
import qualified Control.Monad.Trans.RWS as RWS

import           Data.List (stripPrefix)

import           GHC.Paths (ghc)

import           Info

usage :: String
usage = unlines [
    "Usage:"
  , "  doctest [ --fast | --preserve-it | --no-magic | --stop-on-fail | --verbose | GHC OPTION | MODULE ]..."
  , "  doctest --help"
  , "  doctest --version"
  , "  doctest --info"
  , ""
  , "Options:"
  , "  --fast         disable :reload between example groups"
  , "  --preserve-it  preserve the `it` variable between examples"
  , "  --no-magic     disable magic mode"
  , "  --stop-on-fail stop testing after the first failure"
  , "  --verbose      print each test as it is run"
  , "  --help         display this help and exit"
  , "  --version      output version information and exit"
  , "  --info         output machine-readable version information and exit"
  ]

data Result a = ProxyToGhc [String] | Output String | Result a
  deriving (Eq, Show, Functor)

type Warning = String

data Run = Run {
  runWarnings :: [Warning]
, runMagicMode :: Bool
, runConfig :: Config
} deriving (Eq, Show)

data Config = Config {
  ghcOptions :: [String]
, stopOnFail :: Bool
, fastMode :: Bool
, preserveIt :: Bool
, verbose :: Bool
, repl :: (String, [String])
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {
  ghcOptions = []
, stopOnFail = False
, fastMode = False
, preserveIt = False
, verbose = False
, repl = (ghc, ["--interactive"])
}

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
, runMagicMode = False
, runConfig = defaultConfig
}

modifyWarnings :: ([String] -> [String]) -> Run -> Run
modifyWarnings f run = run { runWarnings = f (runWarnings run) }

setOptions :: [String] -> Run -> Run
setOptions ghcOptions run@Run{..} = run { runConfig = runConfig { ghcOptions } }

setMagicMode :: Bool -> Run -> Run
setMagicMode magic run = run { runMagicMode = magic }

setStopOnFailMode :: Bool -> Run -> Run
setStopOnFailMode stopOnFail run@Run{..} = run { runConfig = runConfig { stopOnFail } }

setFastMode :: Bool -> Run -> Run
setFastMode fastMode run@Run{..} = run { runConfig = runConfig { fastMode } }

setPreserveIt :: Bool -> Run -> Run
setPreserveIt preserveIt run@Run{..} = run { runConfig = runConfig { preserveIt } }

setVerbose :: Bool -> Run -> Run
setVerbose verbose run@Run{..} = run { runConfig = runConfig { verbose } }

parseOptions :: [String] -> Result Run
parseOptions args
  | on "--info" = Output info
  | on "--interactive" = runRunOptionsParser (discard "--interactive" args) defaultRun $ do
      commonRunOptions
  | on `any` nonInteractiveGhcOptions = ProxyToGhc args
  | on "--help" = Output usage
  | on "--version" = Output versionInfo
  | otherwise = runRunOptionsParser args defaultRun {runMagicMode = True} $ do
      commonRunOptions
      parseFlag "--no-magic" (setMagicMode False)
      parseFlag "--stop-on-fail" (setStopOnFailMode True)
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
