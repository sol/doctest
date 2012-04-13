module Report (
  runModules
, Result(..)
, isSucceeded
) where

import           Prelude hiding (putStr, putStrLn, error)
import           Data.Monoid
import           Control.Monad
import           Control.Applicative
import           Control.Exception
import           Text.Printf (printf)
import           System.IO (hPutStrLn, hPutStr, stderr)
import           Data.Char

import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Data.List (intercalate)

import qualified Interpreter
import           Parse hiding (expression, result)
import           Location
import           Count

-- | Summary of a test run.
data Summary = Summary {
  sExamples :: Int
, sTried    :: Int
, sErrors   :: Int
, sFailures :: Int
}

-- | Format a summary.
instance Show Summary where
  show (Summary examples tried errors failures) =
    printf "Examples: %d  Tried: %d  Errors: %d  Failures: %d" examples tried errors failures

-- | Sum up summaries.
instance Monoid Summary where
  mempty = Summary 0 0 0 0
  (Summary x1 x2 x3 x4) `mappend` (Summary y1 y2 y3 y4) = Summary (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4)

-- |
-- The result of doctest.
data Result = Result {
    -- | The number of 'Example's
    rExamples        :: Int
    -- | How many 'Example's are treid to test
  , rTried           :: Int
    -- | How many 'Example's are got errors
  , rErrors          :: Int
    -- | How many 'Example's are got wrong results
  , rFailures        :: Int
    -- | The number of interaction tests
  , rInteractions    :: Int
    -- | Message of failure/error
  , rFailureMessages :: [String]
  }

-- |
-- Checking if 'Result' says that all tests are passed.
isSucceeded :: Result -> Bool
isSucceeded r = rErrors r == 0 && rFailures r == 0

-- |
-- Run all examples from given modules, return true if there were
-- errors/failures.
runModules :: Interpreter.Interpreter -> Bool -> [Module Example] -> IO Result
runModules repl disp modules = do
  let c = countModules' modules
  ReportState _ s ms <- runModules' (exampleCount c) repl disp modules
  return Result {
      rExamples = sExamples s
    , rTried = sTried s
    , rErrors = sErrors s
    , rFailures = sFailures s
    , rInteractions = interactionCount c
    , rFailureMessages = reverse $ filter (not.null) ms
    }

-- |
-- Run all examples from given modules.
runModules' :: Int -> Interpreter.Interpreter -> Bool -> [Module Example] -> IO ReportState
runModules' exN repl disp modules = flip execStateT initialValue $ do
    forM_ modules $ runModule repl disp
    -- report final summary
    when disp $ gets (show . reportStateSummary) >>= report
  where
    initialValue = ReportState 0 mempty {sExamples = exN} []

-- | A monad for generating test reports.
type Report = StateT ReportState IO

data ReportState = ReportState {
  reportStateCount    :: Int     -- ^ column of the end of message
, reportStateSummary  :: Summary -- ^ test summary
, reportStateMessages :: [String]
}

-- | Add output to the report.
report :: String -> Report ()
report msg = do
  overwrite msg
  -- add a newline, this makes the output permanent
  liftIO $ hPutStrLn stderr ""
  modify (\st -> st {reportStateCount = 0})

-- | Add intermediate output to the report.
--
-- This will be overwritten by subsequent calls to `report`/`report_`.
-- Intermediate out may not contain any newlines.
report_ :: String -> Report ()
report_ msg = do
  overwrite msg
  modify (\st -> st {reportStateCount = length msg})

-- | Add output to the report, overwrite any intermediate out.
overwrite :: String -> Report ()
overwrite msg = do
  n <- gets reportStateCount
  let str | 0 < n     = "\r" ++ msg ++ replicate (n - length msg) ' '
          | otherwise = msg
  liftIO (hPutStr stderr str)

-- | Run all examples from given module.
runModule :: Interpreter.Interpreter -> Bool -> Module Example -> Report ()
runModule repl disp (Module name examples) =
  forM_ examples $ \e -> do

    -- report intermediate summary
    when disp $ gets (show . reportStateSummary) >>= report_

    r <- liftIO $ runExample repl name e
    case r of
      Success ->
        success r
      Error   (Located loc (Interaction expression _)) err -> do
        when disp $ do
          report (printf "### Error in %s: expression `%s'" (show loc) expression)
          report err
        error r
      Failure (Located loc (Interaction expression expected)) actual -> do
        when disp $ do
          report (printf "### Failure in %s: expression `%s'" (show loc) expression)
          reportFailure expected actual
        failure r
  where
    success = updateState (Summary 0 1 0 0)
    failure = updateState (Summary 0 1 0 1)
    error   = updateState (Summary 0 1 1 0)

    updateState summary r = do
      ReportState n s rs <- get
      put $ ReportState n (s `mappend` summary) (toMessage r:rs)

reportFailure :: [String] -> [String] -> Report ()
reportFailure expected actual = do
  outputLines "expected: " expected
  outputLines " but got: " actual
  where

    -- print quotes if any line ends with trailing whitespace
    printQuotes = any isSpace (map last . filter (not . null) $ expected ++ actual)

    -- use show to escape special characters in output lines if any output line
    -- contains any unsafe character
    escapeOutput = any (not . isSafe) (concat $ expected ++ actual)

    isSafe :: Char -> Bool
    isSafe c = c == ' ' || (isPrint c && (not . isSpace) c)

    outputLines message l_ = case l of
      x:xs -> do
        report (message ++ x)
        let padding = replicate (length message) ' '
        forM_ xs $ \y -> report (padding ++ y)
      []   ->
        report message
      where
        l | printQuotes || escapeOutput = map show l_
          | otherwise                   = l_

-- | The result of evaluating an interaction.
data InteractionResult =
    Success
  | Failure (Located Interaction) [String]
  | Error (Located Interaction) String

toMessage :: InteractionResult -> String
toMessage Success = "" -- to be filtered
toMessage (Error (Located loc (Interaction expression _)) err) =
    printf "%s: expression `%s'\nError: " (show loc) expression err
toMessage (Failure (Located loc (Interaction expression expected)) actual) =
    printf "%s: expression `%s'\n  expected: %s\n   but got: %s" (show loc) expression (flat expected) (flat actual)
  where
    flat = intercalate "\n"

-- |
-- Execute all expressions from given 'Example' in given
-- 'Interpreter.Interpreter' and verify the output.
--
-- The interpreter state is zeroed with @:reload@ before executing the
-- expressions.  This means that you can reuse the same
-- 'Interpreter.Interpreter' for several calls to `runExample`.
runExample :: Interpreter.Interpreter -> String -> Example -> IO InteractionResult
runExample repl module_ (Example interactions) = do
  _ <- Interpreter.eval repl ":reload"
  _ <- Interpreter.eval repl $ ":m *" ++ module_
  go interactions
  where
    go (i@(Located _ (Interaction expression expected)) : xs) = do
      r <- run expression
      case r of
        Left err     -> return (Error i err)
        Right actual -> if expected /= actual then
                            return (Failure i actual)
                        else
                            go xs
    go [] = return Success

    run :: String -> IO (Either String [String])
    run expression = (Right . lines <$> Interpreter.eval repl expression) `catches` [
      -- Re-throw AsyncException, otherwise execution will not terminate on
      -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
      -- UserInterrupt) because all of them indicate severe conditions and
      -- should not occur during normal test runs.
      Handler $ \e -> throw (e :: AsyncException),

      Handler $ \e -> (return . Left . show) (e :: SomeException)
      ]
