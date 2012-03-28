module Report (
  runModules

-- * exported for testing
, Report
, Summary(..)
, ReportState (..)
, report
, report_
) where

import           Prelude hiding (putStr, putStrLn)
import           Data.Monoid
import           Control.Monad
import           Text.Printf (printf)
import           System.IO (hPutStrLn, hPutStr, stderr)

import           Control.Monad.Trans.State
import           Control.Monad.IO.Class

import qualified Interpreter
import           Parse hiding (expression, result)
import           Location

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

-- | The result of evaluating an interaction.
data InteractionResult = Success | Failed (Located Interaction) [String]

-- |
-- Run all examples from given modules, return true if there were
-- errors/failures.
runModules :: Int -> Interpreter.Interpreter -> [Module Example] -> IO Bool
runModules exampleCount repl modules = do
  ReportState _ s <- (`execStateT` ReportState 0 mempty {sExamples = exampleCount}) $ do
    forM_ modules $ runModule repl

    -- report final summary
    gets (show . reportStateSummary) >>= report

  return (sErrors s /= 0 || sFailures s /= 0)

-- | A monad for generating test reports.
type Report = StateT ReportState IO

data ReportState = ReportState {
  reportStateCount   :: Int     -- ^ characters on the current line
, reportStateSummary :: Summary -- ^ test summary
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
runModule :: Interpreter.Interpreter -> Module Example -> Report ()
runModule repl (Module name examples) = do
  forM_ examples $ \e -> do

    -- report intermediate summary
    gets (show . reportStateSummary) >>= report_

    r <- liftIO $ runExample repl name e
    case r of
      Success ->
        success
      Failed (Located loc (Interaction expression expected)) actual -> do
        report (printf "### Failure in %s: expression `%s'" (show loc) expression)
        report ("expected: " ++ show expected)
        report (" but got: " ++ show actual)
        failure
  where
    success = updateSummary (Summary 0 1 0 0)
    failure = updateSummary (Summary 0 1 0 1)

    updateSummary summary = do
      ReportState n s <- get
      put (ReportState n $ s `mappend` summary)

-- |
-- Execute all expressions from given 'Example' in given
-- 'Interpreter.Interpreter' and verify the output.
--
-- The interpreter state is zeroed with @:reload@ before executing the
-- expressions.  This means that you can reuse the same
-- 'Interpreter.Interpreter' for several calls to `runExample`.
runExample :: Interpreter.Interpreter -> String -> Example -> IO InteractionResult
runExample repl module_ (Example interactions) = do
  _ <- Interpreter.eval repl $ ":reload"
  _ <- Interpreter.eval repl $ ":m *" ++ module_
  go interactions
  where
    go (i@(Located _ (Interaction expression expected)) : xs) = do
      actual <- lines `fmap` Interpreter.eval repl expression
      if expected /= actual
        then
          return (Failed i actual)
        else
          go xs
    go [] = return Success
