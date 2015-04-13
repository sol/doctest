module InterpreterSpec (main, spec) where

import           Test.Hspec

import           Control.Applicative

import           Interpreter (interpreterSupported, haveInterpreterKey, ghcInfo)

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
