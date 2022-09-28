{-# LANGUAGE CPP #-}

module Interpreter (
  Interpreter
, safeEval
, safeEvalIt
, withInterpreter
, ghc
, interpreterSupported

-- exported for testing
, ghcInfo
, haveInterpreterKey
, filterExpression
) where

import           System.Process
import           System.Directory (getPermissions, executable)
import           Control.Monad
import           Control.Exception hiding (handle)
import           Data.Char
import           GHC.Paths (ghc)

import           Language.Haskell.GhciWrapper

haveInterpreterKey :: String
haveInterpreterKey = "Have interpreter"

ghcInfo :: IO [(String, String)]
ghcInfo = read <$> readProcess ghc ["--info"] []

interpreterSupported :: IO Bool
interpreterSupported = do
  -- in a perfect world this permission check should never fail, but I know of
  -- at least one case where it did..
  x <- getPermissions ghc
  unless (executable x) $ do
    fail $ ghc ++ " is not executable!"

  maybe False (== "YES") . lookup haveInterpreterKey <$> ghcInfo

-- | Run an interpreter session.
--
-- Example:
--
-- >>> withInterpreter [] $ \i -> eval i "23 + 42"
-- ...
-- "65\n"
withInterpreter
  :: [String]               -- ^ List of flags, passed to GHC
  -> (Interpreter -> IO a)  -- ^ Action to run
  -> IO a                   -- ^ Result of action
withInterpreter flags action = do
  let
    args = flags ++ [
        "--interactive"
      , xTemplateHaskell
#if __GLASGOW_HASKELL__ >= 802
      , "-fdiagnostics-color=never"
      , "-fno-diagnostics-show-caret"
#endif
      ]
  bracket (new defaultConfig{configGhci = ghc} args) close action

xTemplateHaskell :: String
xTemplateHaskell = "-XTemplateHaskell"

-- | Evaluate an expression; return a Left value on exceptions.
--
-- An exception may e.g. be caused on unterminated multiline expressions.
safeEval :: Interpreter -> String -> IO (Either String String)
safeEval repl = either (return . Left) (fmap Right . eval repl) . filterExpression

safeEvalIt :: Interpreter -> String -> IO (Either String String)
safeEvalIt repl = either (return . Left) (fmap Right . evalIt repl) . filterExpression

filterExpression :: String -> Either String String
filterExpression e =
  case lines e of
    [] -> Right e
    l  -> if firstLine == ":{" && lastLine /= ":}" then err else Right (filterXTemplateHaskell e)
      where
        firstLine = strip $ head l
        lastLine  = strip $ last l
        err = Left "unterminated multi-line command"
  where
    strip :: String -> String
    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

filterXTemplateHaskell :: String -> String
filterXTemplateHaskell input = case words input of
  [":set", setting] | setting == xTemplateHaskell -> ""
  ":set" : xs | xTemplateHaskell `elem` xs -> unwords $ ":set" : filter (/= xTemplateHaskell) xs
  _ -> input
