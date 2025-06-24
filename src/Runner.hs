{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Runner (
  runModules
, FastMode(..)
, PreserveIt(..)
, FailFast(..)
, Verbose(..)
, Summary(..)
, isSuccess
, formatSummary

#ifdef TEST
, Report
, ReportState(..)
, runReport
, Interactive(..)
, report
, reportTransient
#endif
) where

import           Prelude ()
import           Imports hiding (putStr, putStrLn, error)

import           Text.Printf (printf)
import           System.IO hiding (putStr, putStrLn)

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State (StateT, evalStateT)
import qualified Control.Monad.Trans.State as State
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

isSuccess :: Summary -> Bool
isSuccess s = sErrors s == 0 && sFailures s == 0

formatSummary :: Summary -> String
formatSummary (Summary examples tried errors failures) =
  printf "Examples: %d  Tried: %d  Errors: %d  Failures: %d" examples tried errors failures

-- | Sum up summaries.
instance Monoid Summary where
  mempty = Summary 0 0 0 0
instance Semigroup Summary where
  Summary x1 x2 x3 x4 <> Summary y1 y2 y3 y4 = Summary (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4)

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
    run = runReport (ReportState interactive failFast verbose summary) $ do
      reportProgress
      forM_ modules $ runModule fastMode preserveIt repl
      verboseReport "# Final summary:"

  run `finally` reportFinalResult

  readIORef summary
  where
    n :: Int
    n = sum (map countExpressions modules)

countExpressions :: Module [Located DocTest] -> Int
countExpressions (Module _ setup tests) = sum (map length tests) + maybe 0 length setup

type Report = MaybeT (StateT ReportState IO)

data Interactive = NonInteractive | Interactive

data FastMode = NoFastMode | FastMode

data FailFast = NoFailFast | FailFast

data Verbose = NonVerbose | Verbose

data ReportState = ReportState {
  reportStateInteractive :: Interactive
, reportStateFailFast :: FailFast
, reportStateVerbose :: Verbose
, reportStateSummary :: IORef Summary
}

runReport :: ReportState -> Report () -> IO ()
runReport st = void . flip evalStateT st . runMaybeT

getSummary :: Report Summary
getSummary = gets reportStateSummary >>= liftIO . readIORef

gets :: (ReportState -> a) -> Report a
gets = lift . State.gets

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
runModule :: FastMode -> PreserveIt -> Interpreter -> Module [Located DocTest] -> Report ()
runModule fastMode preserveIt repl (Module module_ setup examples) = do

  Summary _ _ e0 f0 <- getSummary

  forM_ setup $
    runTestGroup preserveIt repl reload

  Summary _ _ e1 f1 <- getSummary

  -- only run tests, if setup does not produce any errors/failures
  when (e0 == e1 && f0 == f1) $
    forM_ examples $ runTestGroup preserveIt repl setup_
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
  gets reportStateFailFast >>= \ case
    NoFailFast -> pass
    FailFast -> unless (isSuccess summary) abort

abort :: Report ()
abort = MaybeT $ return Nothing

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
