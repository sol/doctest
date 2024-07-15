{-# LANGUAGE CPP #-}
module Info (
  versionInfo
, info
, version
#ifdef TEST
, formatInfo
#endif
) where

import           Imports

import           Data.List
import           System.Process
import           System.IO.Unsafe

#if __GLASGOW_HASKELL__ < 900
import           Config as GHC
#else
import           GHC.Settings.Config as GHC
#endif

import           Interpreter (ghc)

#ifdef TEST

version :: String
version = "0.0.0"

#else

import           Data.Version (showVersion)
import qualified Paths_doctest

version :: String
version = showVersion Paths_doctest.version

#endif

ghcVersion :: String
ghcVersion = GHC.cProjectVersion

versionInfo :: String
versionInfo = unlines [
    "doctest version " ++ version
  , "using version " ++ ghcVersion ++ " of the GHC API"
  , "using " ++ ghc
  ]

info :: String
info = formatInfo $
    ("version", version)
  : ("ghc_version", ghcVersion)
  : ("ghc", ghc)
  : ghcInfo

type Info = [(String, String)]

ghcInfo :: Info
ghcInfo = read $ unsafePerformIO (readProcess ghc ["--info"] "")

formatInfo :: Info -> String
formatInfo xs = " [" ++ (intercalate "\n ," $ map show xs) ++ "\n ]\n"
