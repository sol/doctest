module Util where

import           Control.Applicative
import           Control.Exception

import           Interpreter (Interpreter)
import qualified Interpreter

convertDosLineEndings :: String -> String
convertDosLineEndings = go
  where
    go input = case input of
      '\r':'\n':xs -> '\n' : go xs

      -- Haddock comments from source files with dos line endings end with a
      -- CR, so we strip that, too.
      "\r"         -> ""

      x:xs         -> x : go xs
      ""           -> ""

-- | Return the longest suffix of elements that satisfy a given predicate.
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse


-- | Evaluate an expression, returning any exceptions as a Left value.
--
-- Exceptions may e.g. be caused on unterminated multiline expressions.
safeEval :: Interpreter -> String -> IO (Either String [String])
safeEval repl expression = (Right . lines <$> Interpreter.eval repl expression) `catches` [
  -- Re-throw AsyncException, otherwise execution will not terminate on
  -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
  -- UserInterrupt) because all of them indicate severe conditions and
  -- should not occur during normal test runs.
  Handler $ \e -> throw (e :: AsyncException),

  Handler $ \e -> (return . Left . show) (e :: SomeException)
  ]
