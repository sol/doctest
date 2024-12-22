{-# LANGUAGE CPP #-}
module Language.Haskell.GhciWrapperSpec (main, spec) where

import           Imports

import           Test.Hspec
import           System.IO.Silently

import           Data.List

import           Language.Haskell.GhciWrapper (Interpreter, Config(..), defaultConfig, PreserveIt(..))
import qualified Language.Haskell.GhciWrapper as Interpreter

main :: IO ()
main = hspec spec

withInterpreterConfig :: Config -> [String] -> (Interpreter -> IO a) -> IO a
withInterpreterConfig config args = bracket (Interpreter.new config args) Interpreter.close

withInterpreterArgs :: [String] -> ((String -> IO String) -> IO a) -> IO a
withInterpreterArgs args action = withInterpreterConfig defaultConfig args $ action . Interpreter.eval

withInterpreter :: ((String -> IO String) -> IO a) -> IO a
withInterpreter = withInterpreterArgs []

spec :: Spec
spec = do
  describe "evalEcho" $ do
    it "prints result to stdout" $ do
      withInterpreterConfig defaultConfig [] $ \ghci -> do
        (capture $ Interpreter.evalEcho ghci ("putStr" ++ show "foo\nbar")) `shouldReturn` ("foo\nbar", "foo\nbar")

  describe "evalWith" $ do
    context "with PreserveIt" $ do
      it "preserves it" $ do
        withInterpreterConfig defaultConfig [] $ \ghci -> do
          Interpreter.evalWith PreserveIt ghci "23" `shouldReturn` "23\n"
          Interpreter.eval ghci "it" `shouldReturn` "23\n"

  describe "eval" $ do
    it "shows literals" $ withInterpreter $ \ghci -> do
      ghci "23" `shouldReturn` "23\n"

    it "shows string literals containing Unicode" $ withInterpreter $ \ghci -> do
      ghci "\"λ\"" `shouldReturn` "\"\\955\"\n"

    it "evaluates simple expressions" $ withInterpreter $ \ghci -> do
      ghci "23 + 42" `shouldReturn` "65\n"

    it "supports let bindings" $ withInterpreter $ \ghci -> do
      ghci "let x = 10" `shouldReturn` ""
      ghci "x" `shouldReturn` "10\n"

    it "allows import statements" $ withInterpreter $ \ghci -> do
      ghci "import Data.Maybe" `shouldReturn` ""
      ghci "fromJust (Just 20)" `shouldReturn` "20\n"

    it "captures stdout" $ withInterpreter $ \ghci -> do
      ghci "putStr \"foo\"" `shouldReturn` "foo"

    it "captures stdout (Unicode)" $ withInterpreter $ \ghci -> do
      ghci "putStrLn \"λ\"" `shouldReturn` "λ\n"

    it "captures stdout (empty line)" $ withInterpreter $ \ghci -> do
      ghci "putStrLn \"\"" `shouldReturn` "\n"

    it "captures stdout (multiple lines)" $ withInterpreter $ \ghci -> do
      ghci "putStrLn \"foo\" >> putStrLn \"bar\" >> putStrLn \"baz\""
        `shouldReturn` "foo\nbar\nbaz\n"

    it "captures stderr" $ withInterpreter $ \ghci -> do
      ghci "import System.IO" `shouldReturn` ""
      ghci "hPutStrLn stderr \"foo\"" `shouldReturn` "foo\n"

    it "captures stderr (Unicode)" $ withInterpreter $ \ghci -> do
      ghci "import System.IO" `shouldReturn` ""
      ghci "hPutStrLn stderr \"λ\"" `shouldReturn` "λ\n"

    it "shows exceptions" $ withInterpreter $ \ghci -> do
      ghci "import Control.Exception" `shouldReturn` ""
#if __GLASGOW_HASKELL__ >= 912
      ghci "throwIO DivideByZero" `shouldReturn` "*** Exception: divide by zero\n\nHasCallStack backtrace:\n  throwIO, called at <interactive>:25:1 in interactive:Ghci22\n\n"
#else
      ghci "throwIO DivideByZero" `shouldReturn` "*** Exception: divide by zero\n"
#endif

    it "shows exceptions (ExitCode)" $ withInterpreter $ \ghci -> do
      ghci "import System.Exit" `shouldReturn` ""
      ghci "exitWith $ ExitFailure 10" `shouldReturn` "*** Exception: ExitFailure 10\n"

    it "gives an error message for identifiers that are not in scope" $ withInterpreter $ \ghci -> do
#if __GLASGOW_HASKELL__ >= 800
      ghci "foo" >>= (`shouldSatisfy` isInfixOf "Variable not in scope: foo")
#elif __GLASGOW_HASKELL__ >= 707
      ghci "foo" >>= (`shouldSatisfy` isSuffixOf "Not in scope: \8216foo\8217\n")
#else
      ghci "foo" >>= (`shouldSatisfy` isSuffixOf "Not in scope: `foo'\n")
#endif
    context "when configVerbose is True" $ do
      it "prints prompt" $ do
        withInterpreterConfig defaultConfig{configVerbose = True} [] $ \ghci -> do
          Interpreter.eval ghci "print 23" >>= (`shouldSatisfy`
            (`elem` [ "Prelude> 23\nPrelude> "
                    ,  "ghci> 23\nghci> "
                    ]))

    context "with -XOverloadedStrings, -Wall and -Werror" $ do
      it "does not fail on marker expression (bug fix)" $ withInterpreter $ \ghci -> do
        ghci ":seti -XOverloadedStrings -Wall -Werror" `shouldReturn` ""
        ghci "putStrLn \"foo\"" `shouldReturn` "foo\n"

    context "with NoImplicitPrelude" $ do
      it "works" $ withInterpreterArgs ["-XNoImplicitPrelude"] $ \ghci -> do
        ghci "putStrLn \"foo\"" >>= (`shouldContain` "Variable not in scope: putStrLn")
        ghci "23" `shouldReturn` "23\n"

    context "with a strange String type" $ do
      it "works" $ withInterpreter $ \ghci -> do
        ghci "type String = Int" `shouldReturn` ""
        ghci "putStrLn \"foo\"" `shouldReturn` "foo\n"
