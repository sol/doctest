{-# LANGUAGE CPP #-}
module InterpreterSpec (main, spec) where

import           Test.Hspec

import           Data.List (isSuffixOf)
import           Control.Applicative

#ifndef mingw32_HOST_OS
import           System.Process (readProcess)
#endif

import qualified Interpreter
import           Interpreter (safeEval, haveInterpreterKey, ghcInfo)

main :: IO ()
main = hspec spec

withInterpreter :: ((String -> IO String) -> IO a) -> IO a
withInterpreter action = Interpreter.withInterpreter [] $ \x -> action (Interpreter.eval x)

shouldEvaluateTo :: (Show a, Eq a) => IO a -> a -> IO ()
action `shouldEvaluateTo` expected = action >>= (`shouldBe` expected)

spec :: Spec
spec = do
  describe "Interpreter" $ do
    it "knows whether it works" $ do
        (Interpreter.interpreterSupported >> return ()) `shouldReturn` ()
   
    it "terminates on SIGINT" $ do
#ifdef mingw32_HOST_OS
      pending
#else
      s <- readProcess "test/interpreter/termination/test_script.sh" [] ""
      s `shouldBe` "success\n"
#endif

  describe "ghcInfo" $ do
    it ("includes " ++ show haveInterpreterKey) $ do
      info <- ghcInfo
      lookup haveInterpreterKey info `shouldSatisfy`
        (||) <$> (== Just "YES") <*> (== Just "NO")

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

    it "treats lines with spaces only as blank" $ withInterpreter $ \ghci -> do
      ghci "putStrLn \" \"" `shouldEvaluateTo` "\n"
      ghci "putStrLn \"foo\" >> putStrLn \"  \" >> putStrLn \"baz\""
        `shouldEvaluateTo` "foo\n\nbaz\n"

    it "captures stderr" $ withInterpreter $ \ghci -> do
      ghci "import System.IO" `shouldEvaluateTo` ""
      ghci "hPutStrLn stderr \"foo\"" `shouldEvaluateTo` "foo\n"

    it "captures stderr (Unicode)" $ withInterpreter $ \ghci -> do
      ghci "import System.IO" `shouldEvaluateTo` ""
      ghci "hPutStrLn stderr \"λ\"" `shouldEvaluateTo` "λ\n"

    it "shows exceptions (undefined)" $ withInterpreter $ \ghci -> do
      ghci "undefined" `shouldEvaluateTo` "*** Exception: Prelude.undefined\n"

    it "shows exceptions (ExitCode)" $ withInterpreter $ \ghci -> do
      ghci "import System.Exit" `shouldEvaluateTo` ""
      ghci "exitWith $ ExitFailure 10" `shouldEvaluateTo` "*** Exception: ExitFailure 10\n"

    it "gives an error message for identifiers that are not in scope" $ withInterpreter $ \ghci -> do
      ghci "foo" >>= (`shouldSatisfy` isSuffixOf "Not in scope: `foo'\n")

  describe "safeEval" $ do
    it "evaluates an expression" $ Interpreter.withInterpreter [] $ \ghci -> do
      safeEval ghci "23 + 42" `shouldReturn` Right "65\n"

    it "returns Left on unterminated multiline command" $ Interpreter.withInterpreter [] $ \ghci -> do
      safeEval ghci ":{\n23 + 42" `shouldReturn` Left "unterminated multiline command"
