{-# LANGUAGE LambdaCase #-}
module Cabal (externalCommand) where

import           Imports

import           Data.List
import           Data.Version (makeVersion)
import           System.IO
import           System.IO.Temp
import           System.Environment
import           System.Exit
import           System.Directory
import           System.FilePath
import           System.Process hiding (system)

import qualified Info
import           Cabal.Paths
import           Cabal.Options

externalCommand :: [String] -> IO ()
externalCommand args = do
  lookupEnv "CABAL" >>= \ case
    Nothing -> run "cabal" args
    Just cabal -> run cabal (drop 1 args)

run :: String -> [String] -> IO ()
run cabal args = do
  rejectUnsupportedOptions args

  Paths{..} <- paths cabal (discardReplOptions args)

  let
    doctest = cache </> "doctest" <> "-" <> Info.version
    script = cache </> "init-ghci-" <> Info.version

  doesFileExist doctest >>= \ case
    True -> pass
    False -> callProcess cabal [
        "install" , "doctest-" <> Info.version
      , "--flag", "-cabal-doctest"
      , "--ignore-project"
      , "--installdir", cache
      , "--program-suffix", "-" <> Info.version
      , "--install-method=copy"
      , "--with-compiler", ghc
      ]

  doesFileExist script >>= \ case
    True -> pass
    False -> writeFileAtomically script ":seti -w -Wdefault"

  callProcess doctest ["--version"]

  let
    repl extraArgs = system cabal ("repl"
      : "--build-depends=QuickCheck"
      : "--build-depends=template-haskell"
      : ("--repl-options=-ghci-script=" <> script)
      : args ++ extraArgs)

  case ghcVersion < makeVersion [9,4] of
    True -> do
      callProcess cabal ("build" : "--only-dependencies" : discardReplOptions args)
      repl ["--with-compiler", doctest, "--with-hc-pkg", ghcPkg]

    False -> do
      withSystemTempDirectory "cabal-doctest" $ \ dir -> do
        repl ["--keep-temp-files", "--repl-multi-file", dir]
        files <- filter (isSuffixOf "-inplace") <$> listDirectory dir
        options <- concat <$> mapM (fmap lines . readFile . combine dir) files
        system doctest ("--no-magic" : options)

system :: FilePath -> [FilePath] -> IO ()
system name args = rawSystem name args >>= \ case
  ExitSuccess -> pass
  err -> exitWith err

writeFileAtomically :: FilePath -> String -> IO ()
writeFileAtomically name contents = do
  (tmp, h) <- openTempFile (takeDirectory name) (takeFileName name)
  hPutStr h contents
  hClose h
  renameFile tmp name
