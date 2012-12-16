module Interpreter (
  Interpreter
, eval
, safeEval
, withInterpreter
, ghc
) where

import           System.IO
import           System.Process
import           System.Exit
import           System.Directory (getPermissions, executable)
import           Control.Monad (when, unless)
import           Control.Exception hiding (handle)
import           Data.Char
import           Data.List

import           Paths (ghc)

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

newInterpreter :: [String] -> IO Interpreter
newInterpreter flags = do

  -- in a perfect world this permission check should never fail, but I know of
  -- at least one case where it did..
  x <- getPermissions ghc
  unless (executable x) $ do
    fail $ ghc ++ " is not executable!"

  (Just stdin_, Just stdout_, Nothing, processHandle ) <- createProcess $ (proc ghc myFlags) {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit}
  setMode stdin_
  setMode stdout_
  let interpreter = Interpreter {hIn = stdin_, hOut = stdout_, process = processHandle}
  _ <- eval interpreter "import System.IO"
  _ <- eval interpreter "import GHC.IO.Handle"
  _ <- eval interpreter "hDuplicateTo stdout stderr"

  -- this is required on systems that don't use utf8 as default encoding (e.g.
  -- Windows)
  _ <- eval interpreter "hSetEncoding stdout utf8"
  _ <- eval interpreter "hSetEncoding stderr utf8"

  return interpreter
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

-- | Evaluate an expression; return a Left value on exceptions.
--
-- An exception may e.g. be caused on unterminated multiline expressions.
safeEval :: Interpreter -> String -> IO (Either String String)
safeEval repl expression = (Right `fmap` Interpreter.eval repl expression) `catches` [
  -- Re-throw AsyncException, otherwise execution will not terminate on
  -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
  -- UserInterrupt) because all of them indicate severe conditions and
  -- should not occur during normal test runs.
  Handler $ \e -> throw (e :: AsyncException),

  Handler $ \e -> (return . Left . show) (e :: SomeException)
  ]
