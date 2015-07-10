{-# LANGUAGE CPP #-}
module Run (
  doctest
, doctestDist
#ifdef TEST
, doctest_
, Summary
, stripOptGhc
, expandDirs
#endif
) where

import           Data.List
import           Control.Applicative ((<$>))
import           Control.Monad (when, unless)
import           System.Directory (doesFileExist, doesDirectoryExist,
                                   getDirectoryContents)
import           System.Environment (getEnvironment)
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath ((</>), takeExtension)
import           System.IO

import qualified Control.Exception as E
import           Panic

import           PackageDBs
import           Parse
import           Help
import           Runner
import qualified Interpreter

-- | Run doctest with given list of arguments.
--
-- Example:
--
-- >>> doctest ["-iexample/src", "example/src/Example.hs"]
-- Examples: 2  Tried: 2  Errors: 0  Failures: 0
--
-- This can be used to create a Cabal test suite that runs doctest for your
-- project.
--
-- If a directory is given, it is traversed to find all .hs and .lhs files
-- inside of it, ignoring hidden entries.
doctest :: [String] -> IO ()
doctest args0
  | "--help"    `elem` args0 = putStr usage
  | "--version" `elem` args0 = printVersion
  | otherwise = do
      args <- concat <$> mapM expandDirs args0
      i <- Interpreter.interpreterSupported
      unless i $ do
        hPutStrLn stderr "WARNING: GHC does not support --interactive, skipping tests"
        exitSuccess

      let (f, args_) = stripOptGhc args
      when f $ do
        hPutStrLn stderr "WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."
        hFlush stderr

      packageDBArgs <- getPackageDBArgs
      let addPackageConf = (packageDBArgs ++)

      r <- doctest_ (addPackageConf args_) `E.catch` \e -> do
        case fromException e of
          Just (UsageError err) -> do
            hPutStrLn stderr ("doctest: " ++ err)
            hPutStrLn stderr "Try `doctest --help' for more information."
            exitFailure
          _ -> E.throwIO e
      when (not $ isSuccess r) exitFailure

-- | Expand a reference to a directory to all .hs and .lhs files within it.
expandDirs :: String -> IO [String]
expandDirs fp = do
    isDir <- doesDirectoryExist fp
    if isDir
        then findHaskellFiles fp
        else return [fp]
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

-- | Same as @doctest@, but sets relevant flags for Cabal projects for
-- cabal_macros.h and the autogen directory.
--
-- Will respect the @HASKELL_DIST_DIR@ environment variable if present (used by
-- stack), otherwise assume a directory named dist (used by cabal-install).
doctestDist :: [String] -> IO ()
doctestDist rest = do
    env <- getEnvironment
    let dist =
            case lookup "HASKELL_DIST_DIR" env of
                Nothing -> "dist"
                Just x -> x
    doctest
        $ concat ["-i", dist, "/build/autogen/"]
        : "-optP-include"
        : concat ["-optP", dist, "/build/autogen/cabal_macros.h"]
        : rest

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

doctest_ :: [String] -> IO Summary
doctest_ args = do

  -- get examples from Haddock comments
  modules <- getDocTests args

  Interpreter.withInterpreter args $ \repl -> do
    runModules repl modules
