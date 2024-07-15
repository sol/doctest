module Cabal.Paths (
  Paths(..)
, paths
) where

import           Imports

import           Data.Char
import qualified Data.List as List
import           Data.Tuple
import           Data.Version hiding (parseVersion)
import qualified Data.Version as Version
import           System.Exit hiding (die)
import           System.Directory
import           System.IO
import           System.Process
import           Text.ParserCombinators.ReadP

import qualified Distribution.Simple.GHC as GHC
import           Distribution.Verbosity
import           Distribution.Simple.Program.Db
import           Distribution.Simple.Program.Types

data Paths = Paths {
  ghc  :: FilePath
, ghcPkg :: FilePath
} deriving (Eq, Show)

paths :: FilePath -> IO Paths
paths cabal = do
  cabalVersion <- strip <$> readProcess cabal ["--numeric-version"] ""

  let
    required :: Version
    required = makeVersion [3, 12]

  when (parseVersion cabalVersion < Just required) $ do
    die $ "'cabal-install' version " <> showVersion required <> " or later is required, but 'cabal --numeric-version' returned " <> cabalVersion <> "."

  values <- parseFields <$> readProcess cabal ["path", "-v0"] ""

  let
    compiler_path :: String
    compiler_path = "compiler-path"

  ghc <- case lookup compiler_path values of
    Nothing -> die $ "Cannot determine the path to 'ghc'. Running 'cabal path' did not return a value for '" <> compiler_path <> "'."
    Just path -> canonicalizePath path

  (_, _, programs) <- GHC.configure silent (Just ghc) Nothing emptyProgramDb

  ghcPkg <- case programPath <$> List.find (programId >>> (== "ghc-pkg")) (configuredPrograms programs) of
    Nothing -> die $ "Cannot determine the path to 'ghc-pkg' from '" <> ghc <> "'."
    Just path -> return path

  return Paths {
    ghc
  , ghcPkg
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
