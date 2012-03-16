module InterpreterSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Data.List (isSuffixOf)
import           System.Process (readProcess)
import           Interpreter (Interpreter)
import qualified Interpreter

main :: IO ()
main = hspecX spec

withInterpreter action = Interpreter.withInterpreter [] $ \x -> action (Interpreter.eval x)

action `shouldEvaluateTo` expected = action >>= (`shouldBe` expected)

spec :: Specs
spec = do
  describe "Interpreter" $ do
    it "terminates on SIGINT" $ do
      s <- readProcess "interpreter/termination/test_script.sh" [] ""
      s `shouldBe` "success\n"

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

    it "shows exceptions (undefined)" $ withInterpreter $ \ghci -> do
      ghci "undefined" `shouldEvaluateTo` "*** Exception: Prelude.undefined\n"

    it "shows exceptions (ExitCode)" $ withInterpreter $ \ghci -> do
      ghci "import System.Exit" `shouldEvaluateTo` ""
      ghci "exitWith $ ExitFailure 10" `shouldEvaluateTo` "*** Exception: ExitFailure 10\n"

    it "gives an error message for identifiers that are not in scope" $ withInterpreter $ \ghci -> do
      ghci "foo" >>= (`shouldSatisfy` isSuffixOf "Not in scope: `foo'\n")
