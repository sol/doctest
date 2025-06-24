{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Run (
  doctest
, doctestWithRepl

, Config(..)
, defaultConfig
, doctestWith

, Result
, Summary(..)
, formatSummary
, isSuccess
, evaluateResult
, doctestWithResult

, runDocTests
#ifdef TEST
, expandDirs
#endif
) where

import           Imports

import           System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import           System.Environment (getEnvironment)
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath ((</>), takeExtension)
import           System.IO
import           System.IO.CodePage (withCP65001)

import qualified Control.Exception as E

#if __GLASGOW_HASKELL__ < 900
import           Panic
#else
import           GHC.Utils.Panic
#endif

import           PackageDBs
import           Parse
import           Options hiding (Result(..))
import qualified Options
import           Runner
import           Location
import qualified Interpreter

-- | Run doctest with given list of arguments.
--
-- Example:
--
-- >>> doctest ["-iexample/src", "example/src/Example.hs"]
-- ...
-- Examples: 2  Tried: 2  Errors: 0  Failures: 0
--
-- This can be used to create a Cabal test suite that runs doctest for your
-- project.
--
-- If a directory is given, it is traversed to find all .hs and .lhs files
-- inside of it, ignoring hidden entries.
doctest :: [String] -> IO ()
doctest = doctestWithRepl (repl defaultConfig)

doctestWithRepl :: (String, [String]) -> [String] -> IO ()
doctestWithRepl repl = interpretResponseFile >=> \ args0 -> case parseOptions args0 of
  Options.ProxyToGhc args -> exec Interpreter.ghc args
  Options.Output s -> putStr s
  Options.Result (Run warnings magicMode config) -> do
    mapM_ (hPutStrLn stderr) warnings
    hFlush stderr

    i <- Interpreter.interpreterSupported
    unless i $ do
      hPutStrLn stderr "WARNING: GHC does not support --interactive, skipping tests"
      exitSuccess

    opts <- case magicMode of
      False -> return (ghcOptions config)
      True -> do
        expandedArgs <- concat <$> mapM expandDirs (ghcOptions config)
        packageDBArgs <- getPackageDBArgs
        addDistArgs <- getAddDistArgs
        return (addDistArgs $ packageDBArgs ++ expandedArgs)
    doctestWith config{repl, ghcOptions = opts}

interpretResponseFile :: [String] -> IO [String]
interpretResponseFile = \ case
  ['@':name] -> lines <$> readFile name
  args -> return args

-- | Expand a reference to a directory to all .hs and .lhs files within it.
expandDirs :: String -> IO [String]
expandDirs fp0 = do
    isDir <- doesDirectoryExist fp0
    if isDir
        then findHaskellFiles fp0
        else return [fp0]
  where
    findHaskellFiles dir = do
        contents <- getDirectoryContents dir
        concat <$> mapM go (filter (not . hidden) contents)
      where
        go name = do
            isDir <- doesDirectoryExist fp
            if isDir
                then findHaskellFiles fp
                else if isHaskellFile fp
                        then return [fp]
                        else return []
          where
            fp = dir </> name

    hidden ('.':_) = True
    hidden _ = False

    isHaskellFile fp = takeExtension fp `elem` [".hs", ".lhs"]

-- | Get the necessary arguments to add the @cabal_macros.h@ file and autogen
-- directory, if present.
getAddDistArgs :: IO ([String] -> [String])
getAddDistArgs = do
    env <- getEnvironment
    let dist = fromMaybe "dist" $ lookup "HASKELL_DIST_DIR" env
        autogen = dist ++ "/build/autogen/"
        cabalMacros = autogen ++ "cabal_macros.h"

    dirExists <- doesDirectoryExist autogen
    if dirExists
        then do
            fileExists <- doesFileExist cabalMacros
            return $ \rest ->
                  concat ["-i", dist, "/build/autogen/"]
                : "-optP-include"
                : (if fileExists
                    then (concat ["-optP", dist, "/build/autogen/cabal_macros.h"]:)
                    else id) rest
        else return id

doctestWith :: Config -> IO ()
doctestWith = doctestWithResult >=> evaluateResult

type Result = Summary

evaluateResult :: Result -> IO ()
evaluateResult r = unless (isSuccess r) exitFailure

doctestWithResult :: Config -> IO Result
doctestWithResult config = do
  (extractDocTests (ghcOptions config) >>= runDocTests config) `E.catch` \e -> do
    case fromException e of
      Just (UsageError err) -> do
        hPutStrLn stderr ("doctest: " ++ err)
        hPutStrLn stderr "Try `doctest --help' for more information."
        exitFailure
      _ -> E.throwIO e

runDocTests :: Config -> [Module [Located DocTest]] -> IO Result
runDocTests Config{..} modules = do
  Interpreter.withInterpreter ((<> ghcOptions) <$> repl) $ \ interpreter -> withCP65001 $ do
    runModules
      (if fastMode then FastMode else NoFastMode)
      (if preserveIt then PreserveIt else NoPreserveIt)
      (if failFast then FailFast else NoFailFast)
      (if verbose then Verbose else NonVerbose)
      interpreter modules
