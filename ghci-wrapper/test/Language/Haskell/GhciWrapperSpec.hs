{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Haskell.GhciWrapperSpec (main, spec) where

import           Test.Hspec
import           System.IO.Silently

import           Control.Exception
import           Data.List (isSuffixOf)

import           Language.Haskell.GhciWrapper (Interpreter, Config(..), defaultConfig)
import qualified Language.Haskell.GhciWrapper as Interpreter

main :: IO ()
main = hspec spec

withInterpreterConfig :: Config -> (Interpreter -> IO a) -> IO a
withInterpreterConfig config action = bracket (Interpreter.new config []) Interpreter.close $ \ghci -> action ghci

withInterpreter :: ((String -> IO String) -> IO a) -> IO a
withInterpreter action = withInterpreterConfig defaultConfig $ \ghci -> action (Interpreter.eval ghci)

shouldEvaluateTo :: (HasCallStack, Show a, Eq a) => IO a -> a -> IO ()
action `shouldEvaluateTo` expected = action >>= flip shouldBe expected

spec :: Spec
spec = do
  describe "evalEcho" $ do
    it "prints result to stdout" $ do
      withInterpreterConfig defaultConfig $ \ghci -> do
        (capture $ Interpreter.evalEcho ghci ("putStr" ++ show "foo\nbar")) `shouldReturn` ("foo\nbar", "foo\nbar")

  describe "evalIt" $ do
    it "preserves it" $ do
      withInterpreterConfig defaultConfig $ \ghci -> do
        Interpreter.evalIt ghci "23" `shouldReturn` "23\n"
        Interpreter.eval ghci "it" `shouldReturn` "23\n"

  describe "eval" $ do
    it "shows literals" $ withInterpreter $ \ghci -> do
      ghci "23" `shouldEvaluateTo` "23\n"

    it "shows string literals containing Unicode" $ withInterpreter $ \ghci -> do
      ghci "\"λ\"" `shouldEvaluateTo` "\"\\955\"\n"

    it "evaluates simple expressions" $ withInterpreter $ \ghci -> do
      ghci "23 + 42" `shouldEvaluateTo` "65\n"

    it "supports let bindings" $ withInterpreter $ \ghci -> do
      ghci "let x = 10" `shouldEvaluateTo` ""
      ghci "x" `shouldEvaluateTo` "10\n"

    it "allows import statements" $ withInterpreter $ \ghci -> do
      ghci "import Data.Maybe" `shouldEvaluateTo` ""
      ghci "fromJust (Just 20)" `shouldEvaluateTo` "20\n"

    it "captures stdout" $ withInterpreter $ \ghci -> do
      ghci "putStr \"foo\"" `shouldEvaluateTo` "foo"

    it "captures stdout (Unicode)" $ withInterpreter $ \ghci -> do
      ghci "putStrLn \"λ\"" `shouldEvaluateTo` "λ\n"

    it "captures stdout (empty line)" $ withInterpreter $ \ghci -> do
      ghci "putStrLn \"\"" `shouldEvaluateTo` "\n"

    it "captures stdout (multiple lines)" $ withInterpreter $ \ghci -> do
      ghci "putStrLn \"foo\" >> putStrLn \"bar\" >> putStrLn \"baz\""
        `shouldEvaluateTo` "foo\nbar\nbaz\n"

    it "captures stderr" $ withInterpreter $ \ghci -> do
      ghci "import System.IO" `shouldEvaluateTo` ""
      ghci "hPutStrLn stderr \"foo\"" `shouldEvaluateTo` "foo\n"

    it "captures stderr (Unicode)" $ withInterpreter $ \ghci -> do
      ghci "import System.IO" `shouldEvaluateTo` ""
      ghci "hPutStrLn stderr \"λ\"" `shouldEvaluateTo` "λ\n"

    it "shows exceptions" $ withInterpreter $ \ghci -> do
      ghci "import Control.Exception" `shouldEvaluateTo` ""
      ghci "throwIO DivideByZero" `shouldEvaluateTo` "*** Exception: divide by zero\n"

    it "shows exceptions (ExitCode)" $ withInterpreter $ \ghci -> do
      ghci "import System.Exit" `shouldEvaluateTo` ""
      ghci "exitWith $ ExitFailure 10" `shouldEvaluateTo` "*** Exception: ExitFailure 10\n"

    it "gives an error message for identifiers that are not in scope" $ withInterpreter $ \ghci -> do
#if __GLASGOW_HASKELL__ >= 800
      ghci "foo" >>= (`shouldSatisfy` isSuffixOf "Variable not in scope: foo\n")
#elif __GLASGOW_HASKELL__ >= 707
      ghci "foo" >>= (`shouldSatisfy` isSuffixOf "Not in scope: \8216foo\8217\n")
#else
      ghci "foo" >>= (`shouldSatisfy` isSuffixOf "Not in scope: `foo'\n")
#endif
    context "when configVerbose is True" $ do
      it "prints prompt" $ do
        withInterpreterConfig defaultConfig{configVerbose = True} $ \ghci -> do
          Interpreter.eval ghci "print 23" `shouldReturn` "Prelude> 23\nPrelude> "
