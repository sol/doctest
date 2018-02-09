{-# LANGUAGE CPP #-}
module Runner (
  runModules
, Summary(..)

#ifdef TEST
, Report
, ReportState (..)
, report
, report_
#endif
) where

import           Prelude hiding (putStr, putStrLn, error)

#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid
import           Control.Applicative
#else
import Data.Semigroup
#endif

import           Control.Monad hiding (forM_)
import           Text.Printf (printf)
import           System.IO (hPutStrLn, hPutStr, stderr, hIsTerminalDevice)
import           Data.Foldable (forM_)

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
  sExamples :: Int
, sTried    :: Int
, sErrors   :: Int
, sFailures :: Int
} deriving Eq

-- | Format a summary.
instance Show Summary where
  show (Summary examples tried errors failures) =
    printf "Examples: %d  Tried: %d  Errors: %d  Failures: %d" examples tried errors failures

-- | Sum up summaries.
instance Semigroup Summary where
  (Summary x1 x2 x3 x4) <> (Summary y1 y2 y3 y4) = Summary (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4)

-- | Sum up summaries.
instance Monoid Summary where
  mempty = Summary 0 0 0 0
  mappend = (<>)

-- | Run all examples from a list of modules.
runModules :: Bool -> Bool -> Interpreter -> [Module [Located DocTest]] -> IO Summary
runModules fastMode preserveIt repl modules = do
  isInteractive <- hIsTerminalDevice stderr
  ReportState _ _ s <- (`execStateT` ReportState 0 isInteractive mempty {sExamples = c}) $ do
    forM_ modules $ runModule fastMode preserveIt repl

    -- report final summary
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
  reportStateCount        :: Int     -- ^ characters on the current line
, reportStateInteractive  :: Bool    -- ^ should intermediate results be printed?
, reportStateSummary      :: Summary -- ^ test summary
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
  f <- gets reportStateInteractive
  when f $ do
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
        -- https://ghc.haskell.org/trac/ghc/ticket/5904, which results in a
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

reportFailure :: Location -> Expression -> Report ()
reportFailure loc expression = do
  report (printf "### Failure in %s: expression `%s'" (show loc) expression)
  updateSummary (Summary 0 1 0 1)

reportError :: Location -> Expression -> String -> Report ()
reportError loc expression err = do
  report (printf "### Error in %s: expression `%s'" (show loc) expression)
  report err
  updateSummary (Summary 0 1 1 0)

reportSuccess :: Report ()
reportSuccess =
  updateSummary (Summary 0 1 0 0)

updateSummary :: Summary -> Report ()
updateSummary summary = do
  ReportState n f s <- get
  put (ReportState n f $ s `mappend` summary)

-- | Run given test group.
--
-- The interpreter state is zeroed with @:reload@ first.  This means that you
-- can reuse the same 'Interpreter' for several test groups.
runTestGroup :: Bool -> Interpreter -> IO () -> [Located DocTest] -> Report ()
runTestGroup preserveIt repl setup tests = do

  -- report intermediate summary
  gets (show . reportStateSummary) >>= report_

  liftIO setup
  runExampleGroup preserveIt repl examples

  forM_ properties $ \(loc, expression) -> do
    r <- liftIO $ do
      setup
      runProperty repl expression
    case r of
      Success ->
        reportSuccess
      Error err -> do
        reportError loc expression err
      Failure msg -> do
        reportFailure loc expression
        report msg
  where
    properties = [(loc, p) | Located loc (Property p) <- tests]

    examples :: [Located Interaction]
    examples = [Located loc (e, r) | Located loc (Example e r) <- tests]

-- |
-- Execute all expressions from given example in given 'Interpreter' and verify
-- the output.
runExampleGroup :: Bool -> Interpreter -> [Located Interaction] -> Report ()
runExampleGroup preserveIt repl = go
  where
    go ((Located loc (expression, expected)) : xs) = do
      r <- fmap lines <$> liftIO (safeEvalWith preserveIt repl expression)
      case r of
        Left err -> do
          reportError loc expression err
        Right actual -> case mkResult expected actual of
          NotEqual err -> do
            reportFailure loc expression
            mapM_ report err
          Equal -> do
            reportSuccess
            go xs
    go [] = return ()

safeEvalWith :: Bool -> Interpreter -> String -> IO (Either String String)
safeEvalWith preserveIt
  | preserveIt = Interpreter.safeEvalIt
  | otherwise  = Interpreter.safeEval
