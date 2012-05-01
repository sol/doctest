module PropertySpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Property
import           Type
import           Location
import           Interpreter (withInterpreter)

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "runProperty" $ do
    it "reports a failing property" $ withInterpreter [] $ \repl -> do
      let expression = noLocation "False"
      runProperty repl expression `shouldReturn` PropertyFailure expression "Falsifiable (after 1 test):"

    it "runs a Bool property" $ withInterpreter [] $ \repl -> do
      runProperty repl (noLocation "True") `shouldReturn` Success

    it "runs a Bool property with an explicit type signature" $ withInterpreter [] $ \repl -> do
      runProperty repl (noLocation "True :: Bool") `shouldReturn` Success

    it "runs an implicitly quantified property" $ withInterpreter [] $ \repl -> do
      runProperty repl (noLocation "(reverse . reverse) xs == (xs :: [Int])") `shouldReturn` Success

    it "runs an implicitly quantified property even with GHC 7.4" $ withInterpreter [] $ \repl -> do
      runProperty repl (noLocation "foldr (+) 0 is == sum (is :: [Int])") `shouldReturn` Success

    it "runs an explicitly quantified property" $ withInterpreter [] $ \repl -> do
      runProperty repl (noLocation "\\xs -> (reverse . reverse) xs == (xs :: [Int])") `shouldReturn` Success

    it "allows to mix implicit and explicit quantification" $ withInterpreter [] $ \repl -> do
      runProperty repl (noLocation "\\x -> x + y == y + x") `shouldReturn` Success

    it "reports the value for which a property fails" $ withInterpreter [] $ \repl -> do
      let expression = noLocation "x == 23"
      runProperty repl expression `shouldReturn` PropertyFailure expression "Falsifiable (after 1 test):  \n0"

    it "reports the values for which a property that takes multiple arguments fails" $ withInterpreter [] $ \repl -> do
      let vals x = case x of (PropertyFailure _ r) -> tail (lines r); _ -> error "Property did not fail!"
      vals `fmap` runProperty repl (noLocation "x == True && y == 10 && z == \"foo\"") `shouldReturn` ["False", "0", show ("" :: String)]

  describe "freeVariables" $ do
    it "finds a free variables in a term" $ withInterpreter [] $ \repl -> do
      freeVariables repl "x" `shouldReturn` ["x"]

    it "ignores duplicates" $ withInterpreter [] $ \repl -> do
      freeVariables repl "x == x" `shouldReturn` ["x"]

    it "works for terms with multiple names" $ withInterpreter [] $ \repl -> do
      freeVariables repl "\\z -> x + y + z == foo 23" `shouldReturn` ["x", "y", "foo"]

    it "works for names that contain a prime" $ withInterpreter [] $ \repl -> do
      freeVariables repl "x' == y''" `shouldReturn` ["x'", "y''"]
