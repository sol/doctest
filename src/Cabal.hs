{-# LANGUAGE LambdaCase #-}
module Cabal (externalCommand) where

import           Imports

import           System.IO
import           System.Environment
import           System.Exit (exitWith)
import           System.Directory
import           System.FilePath
import           System.Process

import qualified Info
import           Cabal.Paths

externalCommand :: [String] -> IO ()
externalCommand args = do
  lookupEnv "CABAL" >>= \ case
    Nothing -> run "cabal" args
    Just cabal -> run cabal (drop 1 args)

run :: String -> [String] -> IO ()
run cabal args = do

  Paths{..} <- paths cabal

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
      , "--with-hc-pkg", ghcPkg
      ]

  doesFileExist script >>= \ case
    True -> pass
    False -> writeFileAtomically script ":seti -w -Wdefault"

  callProcess doctest ["--version"]

  callProcess cabal ("build" : "--only-dependencies" : args)

  spawnProcess cabal ("repl"
    : "--build-depends=QuickCheck"
    : "--build-depends=template-haskell"
    : ("--repl-options=-ghci-script=" <> script)
    : "--with-compiler" : doctest
    : "--with-hc-pkg" : ghcPkg
    : args) >>= waitForProcess >>= exitWith

writeFileAtomically :: FilePath -> String -> IO ()
writeFileAtomically name contents = do
  (tmp, h) <- openTempFile (takeDirectory name) (takeFileName name)
  hPutStr h contents
  hClose h
  renameFile tmp name
