{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Runner (
  runModules
, FastMode(..)
, PreserveIt(..)
, FailFast(..)
, Verbose(..)
, Summary(..)
, formatSummary

#ifdef TEST
, Report
, ReportState(..)
, Interactive(..)
, report
, reportTransient
#endif
) where

import           Prelude ()
import           Imports hiding (putStr, putStrLn, error)

import           Text.Printf (printf)
import           System.IO hiding (putStr, putStrLn)

import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Data.IORef

import           Interpreter (Interpreter, PreserveIt(..), safeEvalWith)
import qualified Interpreter
import           Parse
import           Location
import           Property
import           Runner.Example

-- | Summary of a test run.
data Summary = Summary {
  sExamples :: !Int
, sTried    :: !Int
, sErrors   :: !Int
, sFailures :: !Int
} deriving Eq

instance Show Summary where
  show = formatSummary

formatSummary :: Summary -> String
formatSummary (Summary examples tried errors failures) =
  printf "Examples: %d  Tried: %d  Errors: %d  Failures: %d" examples tried errors failures

-- | Sum up summaries.
instance Monoid Summary where
  mempty = Summary 0 0 0 0
#if __GLASGOW_HASKELL__ < 804
  mappend
#else
instance Semigroup Summary where
  (<>)
#endif
    (Summary x1 x2 x3 x4) (Summary y1 y2 y3 y4) = Summary (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4)

withLineBuffering :: Handle -> IO c -> IO c
withLineBuffering h action = bracket (hGetBuffering h) (hSetBuffering h) $ \ _ -> do
  hSetBuffering h LineBuffering
  action

-- | Run all examples from a list of modules.
runModules :: FastMode -> PreserveIt -> FailFast -> Verbose -> Interpreter -> [Module [Located DocTest]] -> IO Summary
runModules fastMode preserveIt failFast verbose repl modules = withLineBuffering stderr $ do

  interactive <- hIsTerminalDevice stderr <&> \ case
    False -> NonInteractive
    True -> Interactive

  summary <- newIORef mempty {sExamples = n}

  let
    reportFinalResult :: IO ()
    reportFinalResult = do
      final <- readIORef summary
      hPutStrLn stderr (formatSummary final)

    run :: IO ()
    run = flip evalStateT (ReportState interactive verbose summary) $ do
      reportProgress
      forM_ modules $ runModule fastMode preserveIt failFast repl
      verboseReport "# Final summary:"

  run `finally` reportFinalResult

  readIORef summary
  where
    n :: Int
    n = sum (map countExpressions modules)

countExpressions :: Module [Located DocTest] -> Int
countExpressions (Module _ setup tests) = sum (map length tests) + maybe 0 length setup

type Report = StateT ReportState IO

data Interactive = NonInteractive | Interactive

data FastMode = NoFastMode | FastMode

data Verbose = NonVerbose | Verbose

data FailFast = NoFailFast | FailFast

data ReportState = ReportState {
  reportStateInteractive :: Interactive
, reportStateVerbose :: Verbose
, reportStateSummary :: IORef Summary
}

getSummary :: Report Summary
getSummary = gets reportStateSummary >>= liftIO . readIORef

-- | Add output to the report.
report :: String -> Report ()
report = liftIO . hPutStrLn stderr

-- | Add intermediate output to the report.
--
-- This will be overwritten by subsequent calls to `report`/`report_`.
-- Intermediate out may not contain any newlines.
reportTransient :: String -> Report ()
reportTransient msg = gets reportStateInteractive >>= \ case
  NonInteractive -> pass
  Interactive -> liftIO $ do
    hPutStr stderr msg
    hFlush stderr
    hPutStr stderr $ '\r' : (replicate (length msg) ' ') ++ "\r"

-- | Run all examples from given module.
runModule :: FastMode -> PreserveIt -> FailFast -> Interpreter -> Module [Located DocTest] -> Report ()
runModule fastMode preserveIt failFast repl (Module module_ setup examples) = do

  Summary _ _ e0 f0 <- getSummary

  forM_ setup $
    runTestGroup preserveIt repl reload

  Summary _ _ e1 f1 <- getSummary

  -- only run tests, if setup does not produce any errors/failures
  when (e0 == e1 && f0 == f1) $
    runExamples examples
  where
    reload :: IO ()
    reload = do
      case fastMode of
        NoFastMode -> void $ Interpreter.safeEval repl ":reload"
        FastMode -> pass
      void $ Interpreter.safeEval repl $ ":m *" ++ module_

      case preserveIt of
        NoPreserveIt -> pass
        PreserveIt -> do
          -- Evaluate a dumb expression to populate the 'it' variable.
          --
          -- NOTE: This is one reason why we cannot just always use PreserveIt:
          -- 'it' isn't set in a fresh GHCi session.
          void $ Interpreter.safeEval repl $ "()"

    setup_ :: IO ()
    setup_ = do
      reload
      forM_ setup $ \l -> forM_ l $ \(Located _ x) -> case x of
        Property _  -> return ()
        Example e _ -> void $ safeEvalWith preserveIt repl e

    -- run examples - optionally aborting if in failFast mode and a failure occurs
    runExamples :: [[Located DocTest]] -> Report ()
    runExamples [] = return ()
    runExamples (testGroup:moreGroups) = do
      failures <- sFailures <$> getSummary
      case failFast of
        FailFast    -> when (failures == 0) runAndContinue
        NoFailFast  -> runAndContinue
      where
        runAndContinue = do
          runTestGroup preserveIt repl setup_ testGroup
          runExamples moreGroups

reportStart :: Location -> Expression -> String -> Report ()
reportStart loc expression testType = do
  verboseReport (printf "### Started execution at %s.\n### %s:\n%s" (show loc) testType expression)

reportFailure :: Location -> Expression -> [String] -> Report ()
reportFailure loc expression err = do
  report (printf "%s: failure in expression `%s'" (show loc) expression)
  mapM_ report err
  report ""
  updateSummary (Summary 0 1 0 1)

reportError :: Location -> Expression -> String -> Report ()
reportError loc expression err = do
  report (printf "%s: error in expression `%s'" (show loc) expression)
  report err
  report ""
  updateSummary (Summary 0 1 1 0)

reportSuccess :: Report ()
reportSuccess = do
  verboseReport "### Successful!\n"
  updateSummary (Summary 0 1 0 0)

verboseReport :: String -> Report ()
verboseReport msg = gets reportStateVerbose >>= \ case
  NonVerbose -> pass
  Verbose -> report msg

updateSummary :: Summary -> Report ()
updateSummary summary = do
  ref <- gets reportStateSummary
  liftIO $ modifyIORef' ref $ mappend summary
  reportProgress

reportProgress :: Report ()
reportProgress = gets reportStateVerbose >>= \ case
  NonVerbose -> do
    summary <- getSummary
    reportTransient (formatSummary summary)
  Verbose -> pass

-- | Run given test group.
--
-- The interpreter state is zeroed with @:reload@ first.  This means that you
-- can reuse the same 'Interpreter' for several test groups.
runTestGroup :: PreserveIt -> Interpreter -> IO () -> [Located DocTest] -> Report ()
runTestGroup preserveIt repl setup tests = do
  liftIO setup
  runExampleGroup preserveIt repl examples

  forM_ properties $ \(loc, expression) -> do
    r <- do
      liftIO setup
      reportStart loc expression "property"
      liftIO $ runProperty repl expression
    case r of
      Success ->
        reportSuccess
      Error err -> do
        reportError loc expression err
      Failure msg -> do
        reportFailure loc expression [msg]
  where
    properties = [(loc, p) | Located loc (Property p) <- tests]

    examples :: [Located Interaction]
    examples = [Located loc (e, r) | Located loc (Example e r) <- tests]

type Interaction = (Expression, ExpectedResult)

-- |
-- Execute all expressions from given example in given 'Interpreter' and verify
-- the output.
runExampleGroup :: PreserveIt -> Interpreter -> [Located Interaction] -> Report ()
runExampleGroup preserveIt repl = go
  where
    go ((Located loc (expression, expected)) : xs) = do
      reportStart loc expression "example"
      r <- fmap lines <$> liftIO (safeEvalWith preserveIt repl expression)
      case r of
        Left err -> do
          reportError loc expression err
        Right actual -> case mkResult expected actual of
          NotEqual err -> do
            reportFailure loc expression err
          Equal -> do
            reportSuccess
            go xs
    go [] = return ()
