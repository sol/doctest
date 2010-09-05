module Options (
  Option(..)
, getOptions
, ghcOptions
, haddockOptions
) where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit

import System.Console.GetOpt

import qualified Documentation.Haddock as Haddock


data Option = Help
            | Verbose
            | GhcOption String
            deriving (Show, Eq)


optionDescriptions :: [OptDescr Option]
optionDescriptions = [
    Option []     ["help"]        (NoArg Help)                    "display this help and exit"
  , Option []     ["optghc"]      (ReqArg GhcOption "OPTION")     "option to be forwarded to GHC"
  ]


getOptions :: IO ([Option], [String])
getOptions = do
  args  <- getArgs
  let (options, modules, errors) = getOpt Permute optionDescriptions args

  when (Help `elem` options)
    (printAndExit usage)

  when ((not . null) errors)
    (tryHelp $ head errors)

  when (null modules)
    (tryHelp "no input files\n")

  return (options, modules)
  where
    printAndExit :: String -> IO a
    printAndExit s = putStr s >> exitWith ExitSuccess

    usage = usageInfo "Usage: doctest [OPTION]... MODULE...\n" optionDescriptions

    tryHelp message = printAndExit $ "doctest: " ++ message
      ++ "Try `doctest --help' for more information.\n"


-- | Extract all ghc options from given list of options.
--
-- Example:
--
-- >>> ghcOptions [Help, GhcOption "-foo", Verbose, GhcOption "-bar"]
-- ["-foo","-bar"]
ghcOptions :: [Option] -> [String]
ghcOptions opts = [ option | GhcOption option <- opts ]


-- | Format given list of options for Haddock.
haddockOptions :: [Option] -> [Haddock.Flag]
haddockOptions opts = map Haddock.Flag_OptGhc $ ghcOptions opts
