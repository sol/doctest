{-# LANGUAGE TemplateHaskell #-}
module TestInterpreter (tests) where

import Test.Framework.TH (testGroupGenerator)
import Test.HUnit
import Test.Framework.Providers.HUnit

import           Interpreter (Interpreter)
import qualified Interpreter

tests = $(testGroupGenerator)

ghci = Interaction
interpreterTest = assertionFromInteractions

case_local_declaration = interpreterTest
  [ ghci "let x = 10"
    ""
  , ghci "x"
    "10\n"
  ]

case_addition = interpreterTest
  [ ghci "23 + 42"
    "65\n"
  , ghci "putStrLn \"foo\" >> putStrLn \"bar\""
    "foo\nbar\n"
  ]

case_import = interpreterTest
  [ ghci "import Data.Maybe"
    ""
  , ghci "fromJust $ Just 20"
    "20\n"
  ]

case_not_in_scope = interpreterTest
  [ ghci "foo"
    "\n<interactive>:1:1: Not in scope: `foo'\n"
  ]

case_stdout_stderr = interpreterTest
  [ ghci "import System.IO"
    ""
  , ghci "hPutStrLn stdout \"foo\""
    "foo\n"
  , ghci "hPutStrLn stderr \"bar\""
    "bar\n"
  ]

case_show_unicode = interpreterTest
  [ ghci "\"λ\""
    "\"\\955\"\n"
  ]

case_output_unicode = interpreterTest
  [ ghci "putStrLn \"λ\""
    "λ\n"
  ]

case_SystemExit = interpreterTest
  [ ghci "import System.Exit"
    ""
  , ghci "exitWith $ ExitFailure 10"
    "*** Exception: ExitFailure 10\n"
  ]

case_put_empty_line = interpreterTest
  [ ghci "putStrLn \"\""
    "\n"
  ]

case_putStr = interpreterTest
  [ ghci "putStr \"foo\""
    "foo"
  ]

data Interaction =
  Interaction
    String    -- expression
    String    -- result

assertionFromInteractions :: [Interaction] -> Assertion
assertionFromInteractions l = do
  Interpreter.withInterpreter [] $ \repl -> mapM_ (assertionFromInteraction repl) l
  where
    assertionFromInteraction :: Interpreter -> Interaction -> Assertion
    assertionFromInteraction repl (Interaction expression result') = do
      result <- Interpreter.eval repl expression
      assertEqual expression result' result
