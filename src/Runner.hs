{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Runner (
  runModules
, Summary(..)

#ifdef TEST
, Report
, ReportState (..)
, report
, reportTransient
#endif
) where

import           Prelude ()
import           Imports hiding (putStr, putStrLn, error)

import           Text.Printf (printf)
import           System.IO (hGetBuffering, hSetBuffering, BufferMode(..), hFlush, hPutStrLn, hPutStr, stderr, hIsTerminalDevice)

import           Control.Monad.Trans.State
import           Control.Monad.IO.Class

import           Interpreter (Interpreter)
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

-- | Format a summary.
instance Show Summary where
  show (Summary examples tried errors failures) =
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

-- | Run all examples from a list of modules.
runModules :: Bool -> Bool -> Bool -> Interpreter -> [Module [Located DocTest]] -> IO Summary
runModules fastMode preserveIt verbose repl modules = bracket (hGetBuffering stderr) (hSetBuffering stderr) $ \ _ -> do
  hSetBuffering stderr LineBuffering

  isInteractive <- hIsTerminalDevice stderr
  ReportState _ _ s <- (`execStateT` ReportState isInteractive verbose mempty {sExamples = c}) $ do
    forM_ modules $ runModule fastMode preserveIt repl

    verboseReport "# Final summary:"
    gets (show . reportStateSummary) >>= report

  return s
  where
    c = (sum . map count) modules

-- | Count number of expressions in given module.
count :: Module [Located DocTest] -> Int
count (Module _ setup tests) = sum (map length tests) + maybe 0 length setup

-- | A monad for generating test reports.
type Report = StateT ReportState IO

data ReportState = ReportState {
  reportStateInteractive :: Bool -- ^ should intermediate results be printed?
, reportStateVerbose :: Bool
, reportStateSummary :: !Summary -- ^ test summary
}

-- | Add output to the report.
report :: String -> Report ()
report = liftIO . hPutStrLn stderr

-- | Add intermediate output to the report.
--
-- This will be overwritten by subsequent calls to `report`/`report_`.
-- Intermediate out may not contain any newlines.
reportTransient :: String -> Report ()
reportTransient msg = do
  gets reportStateInteractive >>= \ case
    False -> pass
    True -> liftIO $ do
      hPutStr stderr msg
      hFlush stderr
      hPutStr stderr $ '\r' : (replicate (length msg) ' ') ++ "\r"

-- | Run all examples from given module.
runModule :: Bool -> Bool -> Interpreter -> Module [Located DocTest] -> Report ()
runModule fastMode preserveIt repl (Module module_ setup examples) = do

  Summary _ _ e0 f0 <- gets reportStateSummary

  forM_ setup $
    runTestGroup preserveIt repl reload

  Summary _ _ e1 f1 <- gets reportStateSummary

  -- only run tests, if setup does not produce any errors/failures
  when (e0 == e1 && f0 == f1) $
    forM_ examples $
      runTestGroup preserveIt repl setup_
  where
    reload :: IO ()
    reload = do
      unless fastMode $
        -- NOTE: It is important to do the :reload first! See
        -- https://gitlab.haskell.org/ghc/ghc/-/issues/5904, which results in a
        -- panic on GHC 7.4.1 if you do the :reload second.
        void $ Interpreter.safeEval repl ":reload"
      void $ Interpreter.safeEval repl $ ":m *" ++ module_

      when preserveIt $
        -- Evaluate a dumb expression to populate the 'it' variable NOTE: This is
        -- one reason why we cannot have safeEval = safeEvalIt: 'it' isn't set in
        -- a fresh GHCi session.
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
verboseReport xs = do
  verbose <- gets reportStateVerbose
  when verbose $ report xs

updateSummary :: Summary -> Report ()
updateSummary summary = do
  ReportState f v s <- get
  put (ReportState f v $ s `mappend` summary)
  reportProgress

reportProgress :: Report ()
reportProgress = do
  verbose <- gets reportStateVerbose
  when (not verbose) $ gets (show . reportStateSummary) >>= reportTransient

-- | Run given test group.
--
-- The interpreter state is zeroed with @:reload@ first.  This means that you
-- can reuse the same 'Interpreter' for several test groups.
runTestGroup :: Bool -> Interpreter -> IO () -> [Located DocTest] -> Report ()
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
runExampleGroup :: Bool -> Interpreter -> [Located Interaction] -> Report ()
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

safeEvalWith :: Bool -> Interpreter -> String -> IO (Either String String)
safeEvalWith preserveIt
  | preserveIt = Interpreter.safeEvalIt
  | otherwise  = Interpreter.safeEval
