{-# LANGUAGE CPP, OverloadedStrings #-}
module PropertySpec (main, spec) where

import           Test.Hspec
import           Data.String.Builder

import           Property
import           Interpreter (withInterpreter)

main :: IO ()
main = hspec spec

isFailure :: PropertyResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

spec :: Spec
spec = do
  describe "runProperty" $ do
    it "reports a failing property" $ withInterpreter [] $ \repl -> do
      runProperty repl "False" `shouldReturn` Failure "*** Failed! Falsifiable (after 1 test):"

    it "runs a Bool property" $ withInterpreter [] $ \repl -> do
      runProperty repl "True" `shouldReturn` Success

    it "runs a Bool property with an explicit type signature" $ withInterpreter [] $ \repl -> do
      runProperty repl "True :: Bool" `shouldReturn` Success

    it "runs an implicitly quantified property" $ withInterpreter [] $ \repl -> do
      runProperty repl "(reverse . reverse) xs == (xs :: [Int])" `shouldReturn` Success

    it "runs an implicitly quantified property even with GHC 7.4" $
#if __GLASGOW_HASKELL__ == 702
      pendingWith "This triggers a bug in GHC 7.2.*."
      -- try e.g.
      -- >>> 23
      -- >>> :t is
#else
      -- ghc will include a suggestion (did you mean `id` instead of `is`) in
      -- the error message
      withInterpreter [] $ \repl -> do
        runProperty repl "foldr (+) 0 is == sum (is :: [Int])" `shouldReturn` Success
#endif

    it "runs an explicitly quantified property" $ withInterpreter [] $ \repl -> do
      runProperty repl "\\xs -> (reverse . reverse) xs == (xs :: [Int])" `shouldReturn` Success

    it "allows to mix implicit and explicit quantification" $ withInterpreter [] $ \repl -> do
      runProperty repl "\\x -> x + y == y + x" `shouldReturn` Success

    it "reports the value for which a property fails" $ withInterpreter [] $ \repl -> do
      runProperty repl "x == 23" `shouldReturn` Failure "*** Failed! Falsifiable (after 1 test):\n0"

    it "reports the values for which a property that takes multiple arguments fails" $ withInterpreter [] $ \repl -> do
      let vals x = case x of (Failure r) -> tail (lines r); _ -> error "Property did not fail!"
      vals `fmap` runProperty repl "x == True && y == 10 && z == \"foo\"" `shouldReturn` ["False", "0", show ("" :: String)]

    it "defaults ambiguous type variables to Integer" $ withInterpreter [] $ \repl -> do
      runProperty repl "reverse xs == xs" >>= (`shouldSatisfy` isFailure)

  describe "freeVariables" $ do
    it "finds a free variables in a term" $ withInterpreter [] $ \repl -> do
      freeVariables repl "x" `shouldReturn` ["x"]

    it "ignores duplicates" $ withInterpreter [] $ \repl -> do
      freeVariables repl "x == x" `shouldReturn` ["x"]

    it "works for terms with multiple names" $ withInterpreter [] $ \repl -> do
      freeVariables repl "\\z -> x + y + z == foo 23" `shouldReturn` ["x", "y", "foo"]

    it "works for names that contain a prime" $ withInterpreter [] $ \repl -> do
      freeVariables repl "x' == y''" `shouldReturn` ["x'", "y''"]

    it "works for names that are similar to other names that are in scope" $ withInterpreter [] $ \repl -> do
      freeVariables repl "length_" `shouldReturn` ["length_"]

  describe "parseNotInScope" $ do
    context "when error message was produced by GHC 7.4.1" $ do
      it "extracts a variable name of variable that is not in scope from an error message" $ do
        parseNotInScope . build $ do
          "<interactive>:4:1: Not in scope: `x'"
        `shouldBe` ["x"]

      it "ignores duplicates" $ do
        parseNotInScope . build $ do
          "<interactive>:4:1: Not in scope: `x'"
          ""
          "<interactive>:4:6: Not in scope: `x'"
        `shouldBe` ["x"]

      it "works for variable names that contain a prime" $ do
        parseNotInScope . build $ do
          "<interactive>:2:1: Not in scope: x'"
          ""
          "<interactive>:2:7: Not in scope: y'"
        `shouldBe` ["x'", "y'"]

      it "works for error messages with suggestions" $ do
        parseNotInScope . build $ do
          "<interactive>:1:1:"
          "    Not in scope: `is'"
          "    Perhaps you meant `id' (imported from Prelude)"
        `shouldBe` ["is"]

    context "when error message was produced by GHC 8.0.1" $ do
      it "extracts a variable name of variable that is not in scope from an error message" $ do
        parseNotInScope . build $ do
          "<interactive>:1:1: error: Variable not in scope: x"
        `shouldBe` ["x"]

      it "ignores duplicates" $ do
        parseNotInScope . build $ do
          "<interactive>:1:1: error: Variable not in scope: x :: ()"
          ""
          "<interactive>:1:6: error: Variable not in scope: x :: ()"
        `shouldBe` ["x"]

      it "works for variable names that contain a prime" $ do
        parseNotInScope . build $ do
          "<interactive>:1:1: error: Variable not in scope: x' :: ()"
          ""
          "<interactive>:1:7: error: Variable not in scope: y'' :: ()"
        `shouldBe` ["x'", "y''"]

      it "works for error messages with suggestions" $ do
        parseNotInScope . build $ do
          "<interactive>:1:1: error:"
          "    • Variable not in scope: length_"
          "    • Perhaps you meant ‘length’ (imported from Prelude)"
        `shouldBe` ["length_"]
