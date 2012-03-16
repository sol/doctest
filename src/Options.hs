module Options (
  Option(..)
, getOptions
, ghcOptions
) where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStr, stderr)

import System.Console.GetOpt

import Paths_doctest (version)
import Data.Version (showVersion)
import Config as GHC

data Option = Help
            | Version
            | Verbose
            | GhcOption String
            | DumpOnly
            deriving (Show, Eq)


documentedOptions :: [OptDescr Option]
documentedOptions = [
    Option []     ["help"]        (NoArg Help)                    "display this help and exit"
  , Option []     ["version"]     (NoArg Version)                 "output version information and exit"
  , Option ['v']  ["verbose"]     (NoArg Verbose)                 "explain what is being done, enable Haddock warnings"
  , Option []     ["optghc"]      (ReqArg GhcOption "OPTION")     "option to be forwarded to GHC"
  ]

undocumentedOptions :: [OptDescr Option]
undocumentedOptions = [
    Option []     ["dump-only"]   (NoArg DumpOnly)                "dump extracted test cases to stdout"
  ]


getOptions :: IO ([Option], [String])
getOptions = do
  args  <- getArgs
  let (options, modules, errors) = getOpt Permute (documentedOptions ++ undocumentedOptions) args

  when (Help `elem` options) $ do
    putStr usage
    exitSuccess

  when (Version `elem` options) $ do
    putStrLn ("doctest version " ++ showVersion version)
    putStrLn ("using version " ++ GHC.cProjectVersion ++ " of the GHC API")
    exitSuccess

  when ((not . null) errors) $ do
    tryHelp $ head errors

  when (null modules) $ do
    tryHelp "no input files\n"

  return (options, modules)
  where
    usage = usageInfo "Usage: doctest [OPTION]... MODULE...\n" documentedOptions

    tryHelp message = do
      hPutStr stderr $ "doctest: " ++ message ++ "Try `doctest --help' for more information.\n"
      exitFailure


-- | Extract all ghc options from given list of options.
--
-- Example:
--
-- >>> ghcOptions [Help, GhcOption "-foo", Verbose, GhcOption "-bar"]
-- ["-foo","-bar"]
ghcOptions :: [Option] -> [String]
ghcOptions opts = [ option | GhcOption option <- opts ]
