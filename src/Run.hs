{-# LANGUAGE CPP #-}
module Run (
  doctest
, getCppArgs
#ifdef TEST
, doctest_
, Summary
, stripOptGhc
, expandDirs
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Data.List.Compat
import           Control.Monad (when, unless)
import           System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
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

import           Data.Monoid
import           Data.Conduit
import qualified Data.Conduit.Find as F
import           Control.Monad.Trans.Resource

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
  | "--cpp" `elem` args0 = do
      args <-  getCppArgs "."
      doctest' $ args ++ tail args0
  | otherwise = doctest' args0
  where
    doctest' args' = do
      args <- concat <$> mapM expandDirs args'
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
      addDistArgs <- getAddDistArgs

      r <- doctest_ (addDistArgs $ addPackageConf args_) `E.catch` \e -> do
        case fromException e of
          Just (UsageError err) -> do
            hPutStrLn stderr ("doctest: " ++ err)
            hPutStrLn stderr "Try `doctest --help' for more information."
            exitFailure
          _ -> E.throwIO e
      when (not $ isSuccess r) exitFailure


-- | Find "cabal_macros.h", then make include-options to support MIN_VERSION-macro.
getCppArgs ::  String ->  IO [String]
getCppArgs dir = do
  let macroFile = "cabal_macros.h"
      dirname path = take (length path - length macroFile) path
  runResourceT $ F.find dir (F.glob ("*autogen/" ++ macroFile) <> F.regular) $$ do
    v <-  await
    case v of
      Just path ->  return [
          "-i" ++ dirname path
        , "-optP-include"
        , "-optP" ++ path
        ]
      Nothing ->  return []

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
    let dist =
            case lookup "HASKELL_DIST_DIR" env of
                Nothing -> "dist"
                Just x -> x
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
