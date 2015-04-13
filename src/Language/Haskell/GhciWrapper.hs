module Language.Haskell.GhciWrapper (
  Interpreter
, new
, close
, eval
, ghc
) where

import           System.IO
import           System.Process
import           System.Exit
import           Control.Monad
import           Control.Exception
import           Data.List

import           GHC.Paths (ghc)
import           Sandbox (getSandboxArguments)

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

new :: [String] -> IO Interpreter
new flags = do
  sandboxFlags <- getSandboxArguments -- keep this
  let myFlags = ghciFlags ++ flags ++ sandboxFlags
  -- get examples from Haddock comments
  (Just stdin_, Just stdout_, Nothing, processHandle ) <- createProcess $ (proc ghc myFlags) {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit}
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

  return interpreter
  where
    ghciFlags = ["-v0", "--interactive", "-ignore-dot-ghci"]
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
putExpression repl e = do
  hPutStrLn stdin_ e
  hPutStrLn stdin_ marker
  hFlush stdin_
  return ()
  where
    stdin_ = hIn repl


getResult :: Interpreter -> IO String
getResult repl = do
  line <- hGetLine stdout_
  if marker `isSuffixOf` line
    then
      return $ stripMarker line
    else do
      result <- getResult repl
      return $ line ++ '\n' : result
  where
    stdout_ = hOut repl
    stripMarker l = take (length l - length marker) l

-- | Evaluate an expresion
eval
  :: Interpreter
  -> String       -- Expression
  -> IO String    -- Result
eval repl expr = do
  putExpression repl expr
  getResult repl
