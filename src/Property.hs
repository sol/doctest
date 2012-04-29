module Property (
  runProperty
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
  lambda <- toLambda expression
  r <- Interpreter.safeEval repl $ "quickCheck (" ++ lambda ++ ")"
  case r of
    Left err -> do
      return (Error p err)
    Right res
      | "OK, passed" `isInfixOf` res -> return Success
      | otherwise -> do
          let msg =  stripEnd (takeWhileEnd (/= '\b') res)
          return (PropertyFailure p msg)
  where
    -- Currently, GHCi is used to detect free variables.
    -- haskell-src-ext should be used in the future.
    toLambda :: String -> IO String
    toLambda expr = do
      r <- fmap lines `fmap` Interpreter.safeEval repl expr
      case r of
        Right vars
          | any ("Not in scope" `isInfixOf`) vars -> return $ closeTerm expr vars
        _ -> return expr

    -- | Close a given term over a given list of variables.
    closeTerm :: String -> [String] -> String
    closeTerm term vars = "\\" ++ intercalate " " vars' ++ "-> " ++ term
      where
        vars' = map unquote . nub . map (takeWhileEnd (/= ' '))
             . filter ("Not in scope" `isInfixOf`) $ vars
        unquote ('`':xs) = init xs
        unquote xs       = xs
