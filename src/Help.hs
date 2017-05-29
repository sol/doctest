{-# LANGUAGE PackageImports #-}
module Help (
  usage
, printVersion
) where

import           Paths_doctest (version)
import           Data.Version (showVersion)
import "ghc"     Config as GHC
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
        ]

printVersion :: IO ()
printVersion = do
  putStrLn ("doctest version " ++ showVersion version)
  putStrLn ("using version " ++ GHC.cProjectVersion ++ " of the GHC API")
  putStrLn ("using " ++ ghc)
