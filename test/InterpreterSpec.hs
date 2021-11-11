module InterpreterSpec (main, spec) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec

import           Interpreter (interpreterSupported, haveInterpreterKey, ghcInfo, withInterpreter, safeEval, filterExpression)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "interpreterSupported" $ do
    it "indicates whether GHCi is supported on current platform" $ do
      (Interpreter.interpreterSupported >> return ()) `shouldReturn` ()

  describe "ghcInfo" $ do
    it ("includes " ++ show haveInterpreterKey) $ do
      info <- ghcInfo
      lookup haveInterpreterKey info `shouldSatisfy`
        (||) <$> (== Just "YES") <*> (== Just "NO")

  describe "safeEval" $ do
    it "evaluates an expression" $ withInterpreter [] $ \ghci -> do
      Interpreter.safeEval ghci "23 + 42" `shouldReturn` Right "65\n"

    it "returns Left on unterminated multiline command" $ withInterpreter [] $ \ghci -> do
      Interpreter.safeEval ghci ":{\n23 + 42" `shouldReturn` Left "unterminated multi-line command"

  describe "filterExpression" $ do
    it "removes :set -XTemplateHaskell" $ do
      filterExpression ":set -XTemplateHaskell" `shouldBe` Right ""

    it "filters -XTemplateHaskell" $ do
      filterExpression ":set -XTemplateHaskell -XCPP" `shouldBe` Right ":set -XCPP"

    it "leaves :set-statement that do not set -XTemplateHaskell alone " $ do
      filterExpression ":set -XFoo   -XBar" `shouldBe` Right ":set -XFoo   -XBar"
