{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
module Property (
  runProperty
, PropertyResult (..)
#ifdef TEST
, PropertyType (..)
, propertyType
, parseNotInScope
#endif
) where

import           Data.List
import           Data.Maybe
import           Data.Foldable

import           Util
import           Interpreter (Interpreter)
import qualified Interpreter
import           Parse
import           Runner.Example

-- | The result of evaluating an interaction.
data PropertyResult =
    Success
  | Failure String
  | Error String
  deriving (Eq, Show)

-- | The type (not in the "type system" sense) of a property
data PropertyType = Simple | QuickCheck [String] | Unknown
  deriving (Eq, Show)

runProperty :: Interpreter -> Expression -> IO PropertyResult
runProperty repl expression = do
  propertyType repl expression >>= \case
    Simple -> runSimpleProperty repl expression
    QuickCheck vs -> runQuickCheckProperty repl expression vs
    Unknown -> runQuickCheckProperty repl expression []

-- | Run a property with no free variables (that is, no QuickCheck
-- required).
runSimpleProperty :: Interpreter -> Expression -> IO PropertyResult
runSimpleProperty repl expression = do
  r <- fmap lines <$> Interpreter.safeEval repl expression
  case r of
    Left err -> return (Error err)
    Right actual -> case mkResult expected actual of
      NotEqual err -> return $ Failure (unlines err)
      Equal -> return Success
  where
    expected = [ExpectedLine [LineChunk "True"]]

-- | Run a property with one or more free variables using QuickCheck.
runQuickCheckProperty :: Interpreter -> Expression -> [String] -> IO PropertyResult
runQuickCheckProperty repl expression vs = do
  _ <- Interpreter.safeEval repl "import Test.QuickCheck ((==>))"
  _ <- Interpreter.safeEval repl "import Test.QuickCheck.All (polyQuickCheck)"
  _ <- Interpreter.safeEval repl "import Language.Haskell.TH (mkName)"
  _ <- Interpreter.safeEval repl ":set -XTemplateHaskell"
  r <- Interpreter.safeEval repl $ quickCheck expression vs
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


-- | Determing what type of property a term corresponds to.
--
-- GHCi is used to determing the type.
--
-- If the type is Bool the property is simple and does not need QuickCheck.
--
-- Otherwise we assume QuickCheck will be needed. If GHCi reported any free
-- variables they are extracted for passing to QuickCheck.
propertyType :: Interpreter -> String -> IO PropertyType
propertyType repl term = do
  r <- Interpreter.safeEval repl (":type " ++ term)
  case r of
    Left _ -> return Unknown
    Right s -> if " :: Bool\n" `isSuffixOf` s
      then return Simple
      else return $ QuickCheck (parseNotInScope s)

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
#if __GLASGOW_HASKELL__ >= 707
    unquote ('\8216':xs) = init xs
#endif
    unquote xs           = xs
