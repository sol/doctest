{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.GhciWrapper (
  Interpreter
, Config(..)
, defaultConfig
, PreserveIt(..)
, new
, close
, eval
, evalWith
, evalEcho
) where

import           Imports

import           System.IO hiding (stdin, stdout, stderr)
import           System.Process
import           System.Exit
import           Data.List (isSuffixOf)

data Config = Config {
  configGhci :: String
, configVerbose :: Bool
, configIgnoreDotGhci :: Bool
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {
  configGhci = "ghci"
, configVerbose = False
, configIgnoreDotGhci = True
}

data PreserveIt = NoPreserveIt | PreserveIt
  deriving Eq

-- | Truly random marker, used to separate expressions.
--
-- IMPORTANT: This module relies upon the fact that this marker is unique.  It
-- has been obtained from random.org.  Do not expect this module to work
-- properly, if you reuse it for any purpose!
marker :: String
marker = show "dcbd2a1e20ae519a1c7714df2859f1890581d57fac96ba3f499412b2f5c928a1"

itMarker :: String
itMarker = "d42472243a0e6fc481e7514cbc9eb08812ed48daa29ca815844d86010b1d113a"

data Interpreter = Interpreter {
  hIn  :: Handle
, hOut :: Handle
, process :: ProcessHandle
}

new :: Config -> [String] -> IO Interpreter
new Config{..} args_ = do
  (Just stdin_, Just stdout_, Nothing, processHandle ) <- createProcess (proc configGhci args) {
    std_in  = CreatePipe
  , std_out = CreatePipe
  , std_err = Inherit
  }
  setMode stdin_
  setMode stdout_
  let interpreter = Interpreter {hIn = stdin_, hOut = stdout_, process = processHandle}
  evalThrow interpreter "import qualified System.IO"
  evalThrow interpreter "import qualified GHC.IO.Encoding"
  evalThrow interpreter "import qualified GHC.IO.Handle"
  -- The buffering of stdout and stderr is NoBuffering
  evalThrow interpreter "GHC.IO.Handle.hDuplicateTo System.IO.stdout System.IO.stderr"
  -- Now the buffering of stderr is BlockBuffering Nothing
  -- In this situation, GHC 7.7 does not flush the buffer even when
  -- error happens.
  evalThrow interpreter "GHC.IO.Handle.hSetBuffering System.IO.stdout GHC.IO.Handle.LineBuffering"
  evalThrow interpreter "GHC.IO.Handle.hSetBuffering System.IO.stderr GHC.IO.Handle.LineBuffering"

  -- this is required on systems that don't use utf8 as default encoding (e.g.
  -- Windows)
  evalThrow interpreter "GHC.IO.Handle.hSetEncoding System.IO.stdout GHC.IO.Encoding.utf8"
  evalThrow interpreter "GHC.IO.Handle.hSetEncoding System.IO.stderr GHC.IO.Encoding.utf8"

  evalThrow interpreter ":m - System.IO"
  evalThrow interpreter ":m - GHC.IO.Encoding"
  evalThrow interpreter ":m - GHC.IO.Handle"

  return interpreter
  where
    args = args_ ++ catMaybes [
        if configIgnoreDotGhci then Just "-ignore-dot-ghci" else Nothing
      , if configVerbose then Nothing else Just "-v0"
      ]
    setMode h = do
      hSetBinaryMode h False
      hSetBuffering h LineBuffering
      hSetEncoding h utf8

    evalThrow :: Interpreter -> String -> IO ()
    evalThrow interpreter expr = do
      output <- eval interpreter expr
      unless (null output || configVerbose) $ do
        close interpreter
        throwIO (ErrorCall output)

close :: Interpreter -> IO ()
close repl = do
  hClose $ hIn repl

  -- It is crucial not to close `hOut` before calling `waitForProcess`,
  -- otherwise ghci may not cleanly terminate on SIGINT (ctrl-c) and hang
  -- around consuming 100% CPU.  This happens when ghci tries to print
  -- something to stdout in its signal handler (e.g. when it is blocked in
  -- threadDelay it writes "Interrupted." on SIGINT).
  e <- waitForProcess $ process repl
  hClose $ hOut repl

  when (e /= ExitSuccess) $ do
    throwIO (userError $ "Language.Haskell.GhciWrapper.close: Interpreter exited with an error (" ++ show e ++ ")")

putExpression :: Interpreter -> PreserveIt -> String -> IO ()
putExpression Interpreter{hIn = stdin} (equals PreserveIt -> preserveIt) e = do
  hPutStrLn stdin e
  when preserveIt $ hPutStrLn stdin $ "let " ++ itMarker ++ " = it"
  hPutStrLn stdin (marker ++ " :: Data.String.String")
  when preserveIt $ hPutStrLn stdin $ "let it = " ++ itMarker
  hFlush stdin

getResult :: Bool -> Interpreter -> IO String
getResult echoMode Interpreter{hOut = stdout} = go
  where
    go = do
      line <- hGetLine stdout
      if marker `isSuffixOf` line
        then do
          let xs = stripMarker line
          echo xs
          return xs
        else do
          echo (line ++ "\n")
          result <- go
          return (line ++ "\n" ++ result)
    stripMarker l = take (length l - length marker) l

    echo :: String -> IO ()
    echo
      | echoMode = putStr
      | otherwise = \ _ -> return ()

-- | Evaluate an expression
eval :: Interpreter -> String -> IO String
eval = evalWith NoPreserveIt

-- | Like 'eval', but try to preserve the @it@ variable
evalWith :: PreserveIt -> Interpreter -> String -> IO String
evalWith preserveIt repl expr = do
  putExpression repl preserveIt expr
  getResult False repl

-- | Evaluate an expression
evalEcho :: Interpreter -> String -> IO String
evalEcho repl expr = do
  putExpression repl NoPreserveIt expr
  getResult True repl
