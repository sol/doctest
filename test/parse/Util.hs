module Util (runDoctest) where

import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)
import Data.Char (isSpace)


-- | Fork and run a `doctest` process.
runDoctest :: FilePath      -- ^ absolute path to `doctest` binary
           -> FilePath      -- ^ current directory of forked `doctest` process
           -> [String]      -- ^ args, given to `doctest`
           -> IO String     -- ^ stdout of forked `doctest` process
runDoctest doctest workingDir args = do
  cwd <- getCurrentDirectory
  setCurrentDirectory workingDir
  (exit, out, err) <- readProcessWithExitCode doctest args ""
  setCurrentDirectory cwd
  case exit of
    ExitSuccess -> return out
    _           ->
      error $ mayCat "STDERR:" (strip err) ++ mayCat "STDOUT:" (strip out)
      where
        mayCat x y = if null y then "" else unlines ["", x, y]
        strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
