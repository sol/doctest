{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Cabal.Paths (
  Paths(..)
, paths
) where

import           Imports

import           Data.Char
import           Data.Tuple
import           Data.Version hiding (parseVersion)
import qualified Data.Version as Version
import           System.Exit hiding (die)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
import           Text.ParserCombinators.ReadP

data Paths = Paths {
  ghc  :: FilePath
, ghcPkg :: FilePath
, cache :: FilePath
} deriving (Eq, Show)

paths :: FilePath -> [String] -> IO Paths
paths cabal args = do
  cabalVersion <- strip <$> readProcess cabal ["--numeric-version"] ""

  let
    required :: Version
    required = makeVersion [3, 12]

  when (parseVersion cabalVersion < Just required) $ do
    die $ "'cabal-install' version " <> showVersion required <> " or later is required, but 'cabal --numeric-version' returned " <> cabalVersion <> "."

  values <- parseFields <$> readProcess cabal ("path" : args ++ ["-v0"]) ""

  let
    getPath :: String -> String -> IO FilePath
    getPath subject key = case lookup key values of
      Nothing -> die $ "Cannot determine the path to " <> subject <> ". Running 'cabal path' did not return a value for '" <> key <> "'."
      Just path -> canonicalizePath path

  ghc <- getPath "'ghc'" "compiler-path"

  ghcVersion <- strip <$> readProcess ghc ["--numeric-version"] ""

  let
    ghcPkg :: FilePath
    ghcPkg = takeDirectory ghc </> "ghc-pkg-" <> ghcVersion
#ifdef mingw32_HOST_OS
      <.> "exe"
#endif

  doesFileExist ghcPkg >>= \ case
    True -> pass
    False -> die $ "Cannot determine the path to 'ghc-pkg' from '" <> ghc <> "'. File '" <> ghcPkg <> "' does not exist."

  abi <- strip <$> readProcess ghcPkg ["--no-user-package-db", "field", "base", "abi", "--simple-output"] ""

  cache_home <- getPath "Cabal's cache directory" "cache-home"
  let cache = cache_home </> "doctest" </> "ghc-" <> ghcVersion <> "-" <> abi

  createDirectoryIfMissing True cache

  return Paths {
    ghc
  , ghcPkg
  , cache
  }
  where
    parseFields :: String -> [(String, FilePath)]
    parseFields = map parseField . lines

    parseField :: String -> (String, FilePath)
    parseField input = case break (== ':') input of
      (key, ':' : value) -> (key, dropWhile isSpace value)
      (key, _) -> (key, "")

die :: String -> IO a
die message = do
  hPutStrLn stderr "Error: [cabal-doctest]"
  hPutStrLn stderr message
  exitFailure

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseVersion :: String -> Maybe Version
parseVersion = lookup "" . map swap . readP_to_S Version.parseVersion
