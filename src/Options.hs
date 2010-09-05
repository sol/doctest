module Options (
  Option(..)
, parseOptions
, ghcOptions
, haddockOptions
, usage
) where

import System.Console.GetOpt

import qualified Documentation.Haddock as Haddock


data Option = Help
            | Verbose
            | GhcOption String
            deriving (Show, Eq)


options :: [OptDescr Option]
options = [
    Option []     ["help"]        (NoArg Help)                    "display this help and exit"
  , Option []     ["optghc"]      (ReqArg GhcOption "OPTION")     "option to be forwarded to GHC"
  ]


parseOptions :: [String] -> Either String ([Option], [String])
parseOptions args =
   case getOpt Permute options args of
      (opts, files, []) -> Right $ (opts, files)
      (_, _, (firstError : _))    -> Left $ "doctest: " ++ firstError ++ "Try `doctest --help' for more information."


usage :: String
usage = usageInfo header options
  where
    header :: String
    header = "Usage: doctest [OPTION]... MODULE...\n"


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
