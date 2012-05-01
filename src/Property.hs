{-# LANGUAGE CPP #-}
module Property (
  runProperty
#ifdef TEST
, freeVariables
#endif
) where

import           Data.List

import           Util
import           Interpreter (Interpreter)
import qualified Interpreter
import           Type
import           Location
import           Parse

runProperty :: Interpreter -> Located Expression -> IO DocTestResult
runProperty repl p@(Located _ expression) = do
  _ <- Interpreter.eval repl "import Test.QuickCheck (quickCheck, (==>))"
  r <- closeTerm expression >>= (Interpreter.safeEval repl . quickCheck)
  case r of
    Left err -> do
      return (Error p err)
    Right res
      | "OK, passed" `isInfixOf` res -> return Success
      | otherwise -> do
          let msg =  stripEnd (takeWhileEnd (/= '\b') res)
          return (PropertyFailure p msg)
  where
    quickCheck term = "quickCheck (" ++ term ++ ")"

    -- | Find all free variables in given term, and close it by abstrating over
    -- them.
    closeTerm :: String -> IO String
    closeTerm term = do
      r <- freeVariables repl (quickCheck term)
      case r of
        []   -> return term
        vars -> return ("\\" ++ unwords vars ++ "-> (" ++ term ++ ")")

-- | Find all free variables in given term.
--
-- GHCi is used to detect free variables.
freeVariables :: Interpreter -> String -> IO [String]
freeVariables repl term = do
  r <- fmap lines `fmap` Interpreter.safeEval repl (":type " ++ term)
  case r of
    Right err -> do
      return (nub . map extractVariable . filter ("Not in scope: " `isInfixOf`) $ err)
    _ ->
      return []
  where
    -- | Extract variable name from a "Not in scope"-error.
    extractVariable :: String -> String
    extractVariable = unquote . takeWhileEnd (/= ' ')

    -- | Remove quotes from given name, if any.
    unquote ('`':xs) = init xs
    unquote xs       = xs
