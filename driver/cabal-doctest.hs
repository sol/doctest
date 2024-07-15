{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Prelude

import           Data.Version
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process

import           Paths_doctest (version)

main :: IO ()
main = getArgs >>= externalCommand

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

  callProcess cabal [
      "install" , "doctest-" ++ showVersion version
    , "--flag", "-cabal-doctest"
    , "--ignore-project"
    , "--installdir", dir
    , "--install-method=symlink"
    ]

  callProcess (dir </> "doctest") ["--version"]
  callProcess cabal ("build" : "--only-dependencies" : args)
  writeFile script ":seti -w -Wdefault"
  spawnProcess cabal ("repl"
    : "--build-depends=QuickCheck"
    : "--build-depends=template-haskell"
    : ("--repl-options=-ghci-script=" ++ script)
    : "--with-compiler" : doctest
    : args) >>= waitForProcess >>= exitWith
