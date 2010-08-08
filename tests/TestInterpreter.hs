module Main where

import Test.HUnit
import Interpreter

data InterpreterTest =
  InterpreterTest
    String        -- name
    [Interaction] -- interactions

data Interaction =
  Interaction
    String    -- expression
    String    -- result

ghci = Interaction

tests :: [InterpreterTest]
tests = [
    InterpreterTest "testLocalDeclaration" [
      ghci "let x = 10"
      []
    , ghci "x"
      "10\n"
  ]
  , InterpreterTest "testAddition" [
      ghci "23 + 42"
      "65\n"
    , ghci "putStrLn \"foo\" >> putStrLn \"bar\""
      "foo\n\
      \bar\n"
  ]
  , InterpreterTest "testImport" [
      ghci "import Data.Maybe"
      ""
    , ghci "fromJust $ Just 20"
      "20\n"
  ]
  , InterpreterTest "testNotInScope" [
      ghci "foo"
      "\n\
      \<interactive>:1:0: Not in scope: `foo'\n"
  ]
  , InterpreterTest "testStdOutErr" [
      ghci "import System.IO"
      ""
    , ghci "hPutStrLn stdout \"foo\""
      "foo\n"
    , ghci "hPutStrLn stderr \"bar\""
      "bar\n"
  ]
  , InterpreterTest "testShowUnicode" [
      ghci "\"λ\""
      "\"\\955\"\n"
  ]
  , InterpreterTest "testOutputUnicode" [
      ghci "putStrLn \"λ\""
      "λ\n"
  ]
  , InterpreterTest "testSystemExit" [
      ghci "import System.Exit"
      ""
    , ghci "exitWith $ ExitFailure 10"
      "*** Exception: ExitFailure 10\n"
  ]
  , InterpreterTest "testPutEmptyLine" [
      ghci "putStrLn \"\""
      "\n"
  ]
  , InterpreterTest "testPutStr" [
      ghci "putStr \"foo\""
      "foo"
  ]
  ]

main :: IO ()
main = do
  _ <- runTestTT $ TestList $ map testFromInterpreterTest tests
  return ();

testFromInterpreterTest :: InterpreterTest -> Test
testFromInterpreterTest (InterpreterTest name expressions) =
  TestLabel name $ TestCase $ assertionFromInteractions expressions
  where
    assertionFromInteractions :: [Interaction] -> Assertion
    assertionFromInteractions l = do
      withInterpreter [] $ \repl -> mapM_ (assertionFromInteraction repl) l
      where
        assertionFromInteraction :: Interpreter -> Interaction -> Assertion
        assertionFromInteraction repl (Interaction expression result') = do
          result <- eval repl expression
          assertEqual expression result' result
