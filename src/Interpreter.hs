module Interpreter (
  Interpreter
, newInterpreter
, closeInterpreter
, withInterpreter
, eval
) where

import System.IO
import System.Process
import System.Exit
import System.Directory (getPermissions, executable)
import Control.Monad(when)
import Control.Exception (bracket)
import Data.Char
import Data.List

import GHC.Paths (ghc)

-- | Truly random marker, used to separate expressions.
--
-- IMPORTANT: This module relies upon the fact that this marker is unique.  It
-- has been obtained from random.org.  Do not expect this module to work
-- properly, if you reuse it for any purpose!
marker :: String
marker = show "dcbd2a1e20ae519a1c7714df2859f1890581d57fac96ba3f499412b2f5c928a1"

-- | The GHCi process.
data Interpreter = Interpreter {
    hIn  :: Handle
  , hOut :: Handle
  , process :: ProcessHandle
  }

-- | Creating 'Interpreter'
newInterpreter :: [String] -> IO Interpreter
newInterpreter flags = do

  -- in a perfect world this permission check should never fail, but I know of
  -- at least one case where it did..
  x <- getPermissions ghc
  when (not $ executable x) $ do
    fail $ ghc ++ " is not executable!"

  (Just stdin_, Just stdout_, Nothing, processHandle ) <- createProcess $ (proc ghc myFlags) {std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle stdout}
  setMode stdin_
  setMode stdout_
  return Interpreter {hIn = stdin_, hOut = stdout_, process = processHandle}
  where
    myFlags = ["-v0", "--interactive", "-ignore-dot-ghci"] ++ flags

    setMode handle = do
      hSetBinaryMode handle False
      hSetBuffering handle LineBuffering
      hSetEncoding handle utf8


-- | Run an interpreter session.
--
-- Example:
--
-- >>> withInterpreter [] $ \i -> eval i "23 + 42"
-- "65\n"
withInterpreter
  :: [String]               -- ^ List of flags, passed to GHC
  -> (Interpreter -> IO a)  -- ^ Action to run
  -> IO a                   -- ^ Result of action
withInterpreter flags = bracket (newInterpreter flags) closeInterpreter


-- | Closing 'Interpreter'
closeInterpreter :: Interpreter -> IO ()
closeInterpreter repl = do
  hClose $ hIn repl

  -- It is crucial not to close `hOut` before calling `waitForProcess`,
  -- otherwise ghci may not cleanly terminate on SIGINT (ctrl-c) and hang
  -- around consuming 100% CPU.  This happens when ghci tries to print
  -- something to stdout in its signal handler (e.g. when it is blocked in
  -- threadDelay it writes "Interrupted." on SIGINT).
  e <- waitForProcess $ process repl
  hClose $ hOut repl

  when (e /= ExitSuccess) $ error $ "Interpreter exited with an error: " ++ show e 
  return ()

putExpression :: Interpreter -> String -> IO ()
putExpression repl e = do
  hPutStrLn stdin_ $ filterExpression e
  hPutStrLn stdin_ marker
  hFlush stdin_
  return ()
  where
    stdin_ = hIn repl


-- | Fail on unterminated multiline commands.
--
-- Examples:
--
-- >>> filterExpression ""
-- ""
--
-- >>> filterExpression "foobar"
-- "foobar"
--
-- >>> filterExpression ":{"
-- "*** Exception: unterminated multiline command
--
-- >>> filterExpression "  :{  "
-- "*** Exception: unterminated multiline command
--
-- >>> filterExpression "  :{  \nfoobar"
-- "*** Exception: unterminated multiline command
--
-- >>> filterExpression "  :{  \nfoobar \n  :}  "
-- "  :{  \nfoobar \n  :}  "
--
filterExpression :: String -> String
filterExpression e =
  case lines e of
    [] -> e
    l  -> if firstLine == ":{" && lastLine /= ":}" then fail_ else e
      where
        firstLine = strip $ head l
        lastLine  = strip $ last l
        fail_ = error "unterminated multiline command"
  where
    strip :: String -> String
    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse


getResult :: Interpreter -> IO String
getResult repl = do
  line <- hGetLine stdout_
  if isSuffixOf marker line
    then
      return $ stripMarker line
    else do
      result <- getResult repl
      return $ line ++ '\n' : result
  where
    stdout_ = hOut repl
    stripMarker l = take (length l - length marker) l

-- | Evaluate an expression.
eval
  :: Interpreter
  -> String       -- Expression
  -> IO String    -- Result
eval repl expr = do
  putExpression repl expr
  getResult repl
