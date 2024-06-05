module Main (main) where

import GHC.Paths (ghc_pkg)
import System.Environment (getArgs, getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (exitWith)
import qualified System.Process as P
import Test.DocTest (doctest)
import Prelude

main :: IO ()
main = getArgs >>= dispatch

{- | Check if @$CABAL@ is defined to see if used as cabal external command or as
a compiler, then run corresponding actions.
-}
dispatch :: [String] -> IO ()
dispatch args = do
    mCabal <- lookupEnv cabalEnvVariable
    case mCabal of
        Nothing -> doctest args
        -- Drop first arg
        -- See: https://cabal.readthedocs.io/en/latest/external-commands.html
        Just cabal -> cabalRepl cabal (drop 1 args)

-- | Run as external cabal command: run cabal repl with relevant compiler options
cabalRepl :: String -> [String] -> IO ()
cabalRepl cabal args = do
    exePath <- getExecutablePath
    -- Remove `cabalEnvVariable` from the environment, else we will get an infinite loop
    cur_env <- getEnvironment
    let new_env = filter ((/= cabalEnvVariable) . fst) cur_env
    -- Run doctest via cabal repl
    exitCode <-
        P.withCreateProcess
            (P.proc cabal (mkArgs exePath args)){P.env = Just new_env}
            (\_ _ _ p -> P.waitForProcess p)
    exitWith exitCode
  where
    mkArgs exePath opts = baseOpts exePath ++ opts
    baseOpts exePath = ["repl", "--with-compiler", exePath, "--with-hc-pkg", ghc_pkg]

-- | Environment variable passed by cabal when calling external command
cabalEnvVariable :: String
cabalEnvVariable = "CABAL"
