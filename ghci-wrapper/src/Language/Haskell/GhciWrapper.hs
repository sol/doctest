{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.GhciWrapper (
  Interpreter
, Config(..)
, defaultConfig
, new
, close
, eval
) where

import           System.IO hiding (stdin, stdout, stderr)
import           System.Process
import           System.Exit
import           Control.Monad
import           Control.Exception
import           Data.List
import           Data.Maybe

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

-- | Truly random marker, used to separate expressions.
--
-- IMPORTANT: This module relies upon the fact that this marker is unique.  It
-- has been obtained from random.org.  Do not expect this module to work
-- properly, if you reuse it for any purpose!
marker :: String
marker = show "dcbd2a1e20ae519a1c7714df2859f1890581d57fac96ba3f499412b2f5c928a1"

data Interpreter = Interpreter {
    hIn  :: Handle
  , hOut :: Handle
  , process :: ProcessHandle
  }

new :: Config -> [String] -> IO Interpreter
new Config{..} args_ = do
  (Just stdin_, Just stdout_, Nothing, processHandle ) <- createProcess $ (proc configGhci args) {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit}
  setMode stdin_
  setMode stdout_
  let interpreter = Interpreter {hIn = stdin_, hOut = stdout_, process = processHandle}
  _ <- eval interpreter "import System.IO"
  _ <- eval interpreter "import GHC.IO.Handle"
  -- The buffering of stdout and stderr is NoBuffering
  _ <- eval interpreter "hDuplicateTo stdout stderr"
  -- Now the buffering of stderr is BlockBuffering Nothing
  -- In this situation, GHC 7.7 does not flush the buffer even when
  -- error happens.
  _ <- eval interpreter "hSetBuffering stdout LineBuffering"
  _ <- eval interpreter "hSetBuffering stderr LineBuffering"

  -- this is required on systems that don't use utf8 as default encoding (e.g.
  -- Windows)
  _ <- eval interpreter "hSetEncoding stdout utf8"
  _ <- eval interpreter "hSetEncoding stderr utf8"

  _ <- eval interpreter ":m - System.IO"
  _ <- eval interpreter ":m - GHC.IO.Handle"

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

putExpression :: Interpreter -> String -> IO ()
putExpression Interpreter{hIn = stdin} e = do
  hPutStrLn stdin e
  hPutStrLn stdin marker
  hFlush stdin

getResult :: Interpreter -> IO String
getResult Interpreter{hOut = stdout} = go
  where
    go = do
      line <- hGetLine stdout
      if marker `isSuffixOf` line
        then
          return (stripMarker line)
        else do
          result <- go
          return (line ++ "\n" ++ result)
    stripMarker l = take (length l - length marker) l

-- | Evaluate an expresion
eval :: Interpreter -> String -> IO String
eval repl expr = do
  putExpression repl expr
  getResult repl
