{-# LANGUAGE CPP #-}
module Property (
  runProperty
, PropertyResult (..)
#ifdef TEST
, freeVariables
, parseNotInScope
#endif
) where

import           Data.List

import           Util
import           Interpreter (Interpreter)
import qualified Interpreter
import           Parse

-- | The result of evaluating an interaction.
data PropertyResult =
    Success
  | Failure String
  | Error String
  deriving (Eq, Show)

runProperty :: Interpreter -> Expression -> IO PropertyResult
runProperty repl expression = do
  _ <- Interpreter.eval repl "import Test.QuickCheck (quickCheck, (==>))"
  r <- closeTerm expression >>= (Interpreter.safeEval repl . quickCheck)
  case r of
    Left err -> do
      return (Error err)
    Right res
      | "OK, passed" `isInfixOf` res -> return Success
      | otherwise -> do
          let msg =  stripEnd (takeWhileEnd (/= '\b') res)
          return (Failure msg)
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
  r <- Interpreter.safeEval repl (":type " ++ term)
  return (either (const []) (nub . parseNotInScope) r)

-- | Parse and return all variables that are not in scope from a ghc error
-- message.
--
-- >>> parseNotInScope "<interactive>:4:1: Not in scope: `foo'"
-- ["foo"]
parseNotInScope :: String -> [String]
parseNotInScope = nub . map extractVariable . filter ("Not in scope: " `isInfixOf`) . lines
  where
    -- | Extract variable name from a "Not in scope"-error.
    extractVariable :: String -> String
    extractVariable = unquote . takeWhileEnd (/= ' ')

    -- | Remove quotes from given name, if any.
    unquote ('`':xs) = init xs
    unquote xs       = xs
