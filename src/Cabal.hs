{-# LANGUAGE LambdaCase #-}
module Cabal (externalCommand) where

import           Imports

import           System.Environment
import           System.Exit (exitWith)
import           System.FilePath
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process

import qualified Info
import           Cabal.Paths

externalCommand :: [String] -> IO ()
externalCommand args = do
  lookupEnv "CABAL" >>= \ case
    Nothing -> run "cabal" args
    Just cabal -> run cabal (drop 1 args)

run :: String -> [String] -> IO ()
run cabal args = withSystemTempDirectory "doctest" $ \ dir -> do
  let
    doctest = dir </> "doctest"
    script = dir </> "init-ghci"

  Paths{..} <- paths cabal

  callProcess cabal [
      "install" , "doctest-" <> Info.version
    , "--flag", "-cabal-doctest"
    , "--ignore-project"
    , "--installdir", dir
    , "--install-method=symlink"
    , "--with-compiler", ghc
    , "--with-hc-pkg", ghcPkg
    ]

  callProcess (dir </> "doctest") ["--version"]
  callProcess cabal ("build" : "--only-dependencies" : args)
  writeFile script ":seti -w -Wdefault"
  spawnProcess cabal ("repl"
    : "--build-depends=QuickCheck"
    : "--build-depends=template-haskell"
    : ("--repl-options=-ghci-script=" <> script)
    : "--with-compiler" : doctest
    : "--with-hc-pkg" : ghcPkg
    : args) >>= waitForProcess >>= exitWith
