{-# LANGUAGE CPP #-}
module Interpreter (
  Interpreter
, PreserveIt(..)
, safeEval
, safeEvalWith
, withInterpreter
, ghc
, interpreterSupported

-- exported for testing
, ghcInfo
, haveInterpreterKey
, filterExpression
) where

import           Imports

import           System.Process
import           System.Directory (getPermissions, executable)
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

  (== Just "YES") . lookup haveInterpreterKey <$> ghcInfo

withInterpreter
  :: (String, [String])
  -> (Interpreter -> IO a)  -- ^ Action to run
  -> IO a                   -- ^ Result of action
withInterpreter (command, flags) action = do
  let
    args = flags ++ [
        xTemplateHaskell
#if __GLASGOW_HASKELL__ >= 802
      , "-fdiagnostics-color=never"
      , "-fno-diagnostics-show-caret"
#endif
#if __GLASGOW_HASKELL__ >= 810 && __GLASGOW_HASKELL__ < 904
      , "-Wno-unused-packages"
#endif
#if __GLASGOW_HASKELL__ >= 910
      , "-fprint-error-index-links=never"
#endif
      ]
  bracket (new defaultConfig{configGhci = command} args) close action

xTemplateHaskell :: String
xTemplateHaskell = "-XTemplateHaskell"

-- | Evaluate an expression; return a Left value on exceptions.
--
-- An exception may e.g. be caused on unterminated multiline expressions.
safeEval :: Interpreter -> String -> IO (Either String String)
safeEval = safeEvalWith NoPreserveIt

safeEvalWith :: PreserveIt -> Interpreter -> String -> IO (Either String String)
safeEvalWith preserveIt repl = either (return . Left) (fmap Right . evalWith preserveIt repl) . filterExpression

filterExpression :: String -> Either String String
filterExpression e =
  case lines e of
    [] -> Right e
    l  -> if firstLine == ":{" && lastLine /= ":}" then err else Right (filterXTemplateHaskell e)
      where
        firstLine = strip $ head l
        lastLine  = strip $ last l
        err = Left "unterminated multi-line command"

filterXTemplateHaskell :: String -> String
filterXTemplateHaskell input = case words input of
  [":set", setting] | setting == xTemplateHaskell -> ""
  ":set" : xs | xTemplateHaskell `elem` xs -> unwords $ ":set" : filter (/= xTemplateHaskell) xs
  _ -> input
