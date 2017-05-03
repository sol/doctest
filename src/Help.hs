module Help (
  usage
, printVersion
, printNumericVersion
, printGhcNumericVersion
) where

import           Paths_doctest (version)
import           Data.Version (showVersion)
import           Config as GHC
import           Interpreter (ghc)

usage :: String
usage = unlines [
          "Usage:"
        , "  doctest [ GHC OPTION | MODULE ]..."
        , "  doctest --help"
        , "  doctest --version"
        , ""
        , "Options:"
        , "  --help     display this help and exit"
        , "  --version  output version information and exit"
        , "  --numeric-version"
        , "             output just the version"
        , "  --ghc-numeric-version"
        , "             output the ghc version identical to ghc --numeric-version"
        ]

printVersion :: IO ()
printVersion = do
  putStrLn ("doctest version " ++ showVersion version)
  putStrLn ("using version " ++ GHC.cProjectVersion ++ " of the GHC API")
  putStrLn ("using " ++ ghc)

printNumericVersion :: IO ()
printNumericVersion = putStrLn (showVersion version)

printGhcNumericVersion :: IO ()
printGhcNumericVersion = putStrLn GHC.cProjectVersion
