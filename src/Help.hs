module Help (
  usage
, printVersion
, printGhcVersion
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
        , "  --ghc-version"
        , "             output the ghc version identical to ghc --version"
        ]

printVersion :: IO ()
printVersion = do
  putStrLn ("doctest version " ++ showVersion version)
  putStrLn ("using version " ++ GHC.cProjectVersion ++ " of the GHC API")
  putStrLn ("using " ++ ghc)

printGhcVersion :: IO ()
printGhcVersion =
  putStrLn (GHC.cProjectName ++ ", version " ++ GHC.cProjectVersion)
