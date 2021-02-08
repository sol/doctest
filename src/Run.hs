{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
module Run (
  doctest
#ifdef TEST
, doctestWithOptions
, Summary
, expandDirs
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad (when, unless)
import           Data.Maybe (maybeToList, mapMaybe)
import           Data.List (isPrefixOf)
import           System.Directory
  (doesFileExist, doesDirectoryExist, getDirectoryContents, canonicalizePath)
import           System.Environment (getEnvironment)
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath ((</>), takeExtension, dropFileName)
import           System.IO
import           System.IO.CodePage (withCP65001)

import qualified Control.Exception as E
import           Panic

import           PackageDBs
import           Parse
import           Options
import           Runner
import qualified Interpreter
import           Location

#if __GLASGOW_HASKELL__ > 804
import           Data.Containers.ListUtils (nubOrd)
#else
import qualified Data.Set as Set

nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . Set.fromList
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
--
-- If a directory is given, it is traversed to find all .hs and .lhs files
-- inside of it, ignoring hidden entries.
doctest :: [String] -> IO ()
doctest args0 = case parseOptions args0 of
  Output s -> putStr s
  Result (Run warnings args_ magicMode fastMode preserveIt verbose isolate nThreads usePackageDb) -> do
    mapM_ (hPutStrLn stderr) warnings
    hFlush stderr

    i <- Interpreter.interpreterSupported
    unless i $ do
      hPutStrLn stderr "WARNING: GHC does not support --interactive, skipping tests"
      exitSuccess

    args <- case magicMode of
      False -> return args_
      True -> do
        expandedArgs <- concat <$> mapM expandDirs args_
        packageDBArgs <- getPackageDBArgs
        addDistArgs <- getAddDistArgs
        return (addDistArgs $ packageDBArgs ++ expandedArgs)

    r <- doctestWithOptions fastMode preserveIt verbose isolate nThreads usePackageDb args `E.catch` \e -> do
      case fromException e of
        Just (UsageError err) -> do
          hPutStrLn stderr ("doctest: " ++ err)
          hPutStrLn stderr "Try `doctest --help' for more information."
          exitFailure
        _ -> E.throwIO e
    when (not $ isSuccess r) exitFailure

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

collectLocations :: Module [Located DocTest] -> [String]
collectLocations (Module{moduleSetup,moduleContent}) =
  nubOrd (concatMap (mapMaybe go) (maybeToList moduleSetup <> moduleContent))
 where
  go :: Located DocTest -> Maybe String
  go (Located (Location l _) _) = Just l
  go _ = Nothing

stripLocalDirs :: [Module [Located DocTest]] -> [String] -> IO [String]
stripLocalDirs mods args_ = do
  modLocs <- mapM canonicalizePath (concatMap collectLocations mods)
  let modDirs = map dropFileName modLocs
  go modDirs args_
 where
  go _modDirs [] = pure []
  go modDirs (arg@('-':'i':dir0):args) = do
    dir1 <- canonicalizePath dir0
    if any (dir1 `isPrefixOf`) modDirs
    then go modDirs args
    else (arg:) <$> go modDirs args
  go modDirs (arg:args) = (arg:) <$> go modDirs args

doctestWithOptions :: Bool -> Bool -> Bool -> Bool -> Int -> Bool -> [String] -> IO Summary
doctestWithOptions fastMode preserveIt verbose isolate nThreads usePackageDb args0 = do

  -- get examples and properties from Haddock comments
  modules <- getDocTests args0

  -- Strip all local directories from -i flags to prevent GHCi from loading
  -- from local module. Force it to load the modules from the package db
  -- instead.
  args1 <- if usePackageDb then stripLocalDirs modules args0 else pure args0

  let run replM = runModules fastMode preserveIt verbose nThreads replM modules

  if isolate || nThreads > 1 then
    -- Run each module with its own interpreter
    run (Left args1)
  else
    -- Run each module with same interpreter, potentially creating a dependency
    -- between them.
    Interpreter.withInterpreter args1 $
      \repl -> withCP65001 $ run (Right repl)
