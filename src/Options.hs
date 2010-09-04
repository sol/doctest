module Options (
  Option(..)
, parseOptions
, ghcOptions
, haddockOptions
) where

import System.Console.GetOpt


data Option = HaddockOption String
            | GhcOption String
            deriving (Show)


options :: [OptDescr Option]
options = [
    Option []     ["opthaddock"]  (ReqArg HaddockOption "OPTION") "option to be forwarded to Haddock"
  , Option []     ["optghc"]      (ReqArg GhcOption "OPTION")     "option to be forwarded to GHC"
  ]


parseOptions :: [String] -> Either String ([Option], [String])
parseOptions args =
   case getOpt Permute options args of
      (opts, files, []) -> Right $ (opts, files)
      (_, _, errors)    -> Left $ '\n' : concat errors ++ usage


usage :: String
usage = usageInfo header options
  where
    header :: String
    header = "\nUsage: doctest [OPTION...] file...\n"


-- | Extract all ghc options from given list of options.
--
-- Example:
--
-- >>> ghcOptions [HaddockOption "-foo", GhcOption "-bar", HaddockOption "-baz"]
-- ["-bar"]
ghcOptions :: [Option] -> [String]
ghcOptions opts = filter (not . null) $ map extract opts
  where
    extract (GhcOption x) = x
    extract _             = []


-- | Format given list of options for Haddock.
haddockOptions :: [Option] -> [String]
haddockOptions opts = (map formatOption opts)
  where
    formatOption (HaddockOption x) = x
    formatOption (GhcOption x)     = "--optghc=" ++ x
