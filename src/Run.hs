{-# LANGUAGE CPP #-}
module Run (
  doctest
#ifdef TEST
, doctest_
, Summary
, stripOptGhc
#endif
) where

import           Data.List
import           Control.Monad (when, unless)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO
import           System.Environment (getEnvironment)

import           Control.Applicative
import qualified Control.Exception as E
import           Panic

import           Parse
import           Help
import           Runner
import qualified Interpreter
import           TestSelector 
                 ( extractTestSelectors
                 , filterModules
                 , Args (Args)
                 , TestSelector )

ghcPackageDbFlag :: String
#if __GLASGOW_HASKELL__ >= 706
ghcPackageDbFlag = "-package-db"
#else
ghcPackageDbFlag = "-package-conf"
#endif

-- | Run doctest with given list of arguments.
--
-- Example:
--
-- >>> doctest ["-iexample/src", "example/src/Example.hs"]
-- Examples: 2  Tried: 2  Errors: 0  Failures: 0
--
-- This can be used to create a Cabal test suite that runs doctest for your
-- project.
doctest :: [String] -> IO ()
doctest args
  | "--help"    `elem` args = putStr usage
  | "--version" `elem` args = printVersion
  | otherwise = do
      -- Look up the HASKELL_PACKAGE_SANDBOX environment variable and, if
      -- present, add it to the list of package databases GHC searches.
      -- Intended to make testing from inside sandboxes such as cabal-dev
      -- simpler.
      packageConf <- lookup "HASKELL_PACKAGE_SANDBOX" <$> getEnvironment
      let addPackageConf = case packageConf of
            Nothing -> id
            Just p  -> \rest -> ghcPackageDbFlag : p : rest
      
      i <- Interpreter.interpreterSupported
      unless i $ do
        hPutStrLn stderr "WARNING: GHC does not support --interactive, skipping tests"
        exitSuccess

      either  
        (usageError . show)
        (\ (Args selectors ghcArgs) -> do
          let (f  , args_) = stripOptGhc ghcArgs

          when f $ do
            hPutStrLn stderr "WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."
            hFlush stderr
          r <- doctest_ selectors (addPackageConf args_) `E.catch` \e -> do
            case fromException e of
              Just (UsageError err) -> usageError err
              _ -> E.throwIO e
          when (not $ isSuccess r) exitFailure)
       (extractTestSelectors args)
    where
      usageError err = do
        hPutStrLn stderr ("doctest: " ++ err)
        hPutStrLn stderr "Try `doctest --help' for more information."
        exitFailure

isSuccess :: Summary -> Bool
isSuccess s = sErrors s == 0 && sFailures s == 0

-- |
-- Strip --optghc from GHC options.  This is for backward compatibility with
-- previous versions of doctest.
--
-- A boolean is returned with the stripped arguments.  It is True if striping
-- occurred.
stripOptGhc :: [String] -> (Bool, [String])
stripOptGhc = go
  where
    go args = case args of
      []                      -> (False, [])
      "--optghc" : opt : rest -> (True, opt : snd (go rest))
      opt : rest              -> maybe (fmap (opt :)) (\x (_, xs) -> (True, x :xs)) (stripPrefix "--optghc=" opt) (go rest)

doctest_ :: [TestSelector] -> [String] -> IO Summary
doctest_ testSelectors args = do

  -- get examples from Haddock comments
  modules <- filterModules testSelectors <$> getDocTests args

  Interpreter.withInterpreter args $ \repl -> do
    runModules repl modules
