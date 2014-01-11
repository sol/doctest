module Help (
  usage
, printVersion
) where

import           Paths_doctest (version)
import           Data.Version (showVersion)
import           Config as GHC
import           Interpreter (ghc)

usage :: String
usage = unlines [
          "Usage:"
        , "  doctest [ --dt-select=<Module>:<lineStart>[-lineEnd] | GHC OPTION | MODULE ]..."
        , "  doctest --help"
        , "  doctest --version"
        , ""
        , "Options:"
        , "  --help      display this help and exit"
        , "  --version   output version information and exit"
        , "  --dt-select=<Module>:[<firstLine>[-lastLine]]"
        , "              Selectively run doctests based on Module and line"
        , "              numbers. Can specify more than one of this option."
        , "              e.g: --dt-select=Foo       All tests in Foo"
        , "                   --dt-select=Foo:13    Foo line 13 " 
        , "                   --dt-select=Bar:13-15 Foo lines 13-15"
        ]

printVersion :: IO ()
printVersion = do
  putStrLn ("doctest version " ++ showVersion version)
  putStrLn ("using version " ++ GHC.cProjectVersion ++ " of the GHC API")
  putStrLn ("using " ++ ghc)
