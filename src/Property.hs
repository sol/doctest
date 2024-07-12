{-# LANGUAGE CPP #-}
module Property (
  runProperty
, PropertyResult (..)
#ifdef TEST
, freeVariables
, parseNotInScope
#endif
) where

import           Imports

import           Data.List
import           Data.Foldable

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
  _ <- Interpreter.safeEval repl "import Test.QuickCheck ((==>))"
  _ <- Interpreter.safeEval repl "import Test.QuickCheck.All (polyQuickCheck)"
  _ <- Interpreter.safeEval repl "import Language.Haskell.TH (mkName)"
  r <- freeVariables repl expression >>=
       (Interpreter.safeEval repl . quickCheck expression)
  case r of
    Left err -> do
      return (Error err)
    Right res
      | "OK, passed" `isInfixOf` res -> return Success
      | otherwise -> do
          let msg =  stripEnd (takeWhileEnd (/= '\b') res)
          return (Failure msg)
  where
    quickCheck term vars =
      "let doctest_prop " ++ unwords vars ++ " = " ++ term ++ "\n" ++
      "$(polyQuickCheck (mkName \"doctest_prop\"))"

-- | Find all free variables in given term.
--
-- GHCi is used to detect free variables.
freeVariables :: Interpreter -> String -> IO [String]
freeVariables repl term = do
  r <- Interpreter.safeEval repl (":type " ++ term)
  return (either (const []) (nub . parseNotInScope) r)

-- | Parse and return all variables that are not in scope from a ghc error
-- message.
parseNotInScope :: String -> [String]
parseNotInScope = nub . mapMaybe extractVariable . lines
  where
    -- | Extract variable name from a "Not in scope"-error.
    extractVariable :: String -> Maybe String
    extractVariable x
      | "Not in scope: " `isInfixOf` x = Just . unquote . takeWhileEnd (/= ' ') $ x
      | Just y <- (asum $ map (stripPrefix "Variable not in scope: ") (tails x)) = Just (takeWhile (/= ' ') y)
      | otherwise = Nothing

    -- | Remove quotes from given name, if any.
    unquote ('`':xs)     = init xs
    unquote ('\8216':xs) = init xs
    unquote xs           = xs
