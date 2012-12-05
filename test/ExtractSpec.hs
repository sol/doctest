module ExtractSpec (main, spec) where

import           Test.Hspec
import           Test.HUnit

import           Panic (GhcException (..))

import           Extract
import           Location
import           System.FilePath

import           Orphans ()

shouldGive :: (String, String) -> [Module String] -> Assertion
(d, m) `shouldGive` expected = do
  r <- map (fmap unLoc) `fmap` extract ["-i" ++ dir, dir </> m]
  r `shouldBe` expected
  where dir = "test/extract" </> d

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "extract" $ do
    it "extracts documentation for a top-level declaration" $ do
      ("declaration", "Foo.hs") `shouldGive` [Module "Foo" Nothing [" Some documentation"]]

    it "extracts documentation from argument list" $ do
      ("argument-list", "Foo.hs") `shouldGive` [Module "Foo" Nothing [" doc for arg1", " doc for arg2"]]

    it "extracts documentation for a type class function" $ do
      ("type-class", "Foo.hs") `shouldGive` [Module "Foo" Nothing [" Convert given value to a string."]]

    it "extracts documentation from the argument list of a type class function" $ do
      ("type-class-args", "Foo.hs") `shouldGive` [Module "Foo" Nothing [" foo", " bar"]]

    it "extracts documentation from the module header" $ do
      ("module-header", "Foo.hs") `shouldGive` [Module "Foo" Nothing [" Some documentation"]]

    it "extracts documentation from imported modules" $ do
      ("imported-module", "Bar.hs") `shouldGive` [Module "Bar" Nothing [" documentation for bar"], Module "Baz" Nothing [" documentation for baz"]]

    it "extracts documentation from export list" $ do
      ("export-list", "Foo.hs") `shouldGive` [Module "Foo" Nothing [" documentation from export list"]]

    it "extracts documentation from named chunks" $ do
      ("named-chunks", "Foo.hs") `shouldGive` [Module "Foo" Nothing [" named chunk foo", "\n named chunk bar"]]

    it "returns docstrings in the same order they appear in the source" $ do
      ("comment-order", "Foo.hs") `shouldGive` [Module "Foo" Nothing [" module header", " export list 1", " export list 2", " foo", " named chunk", " bar"]]

    it "extracts $setup code" $ do
      ("setup", "Foo.hs") `shouldGive` [Module "Foo" (Just "\n some setup code") [" foo", " bar", " baz"]]

    it "fails on invalid flags" $ do
      extract ["--foobar", "test/Foo.hs"] `shouldThrow` (\e -> case e of UsageError "unrecognized option `--foobar'" -> True; _ -> False)

  describe "extract (regression tests)" $ do
    it "works with infix operators" $ do
      ("regression", "Fixity.hs") `shouldGive` [Module "Fixity" Nothing []]

    it "works with parallel list comprehensions" $ do
      ("regression", "ParallelListComp.hs") `shouldGive` [Module "ParallelListComp" Nothing []]

    it "works with list comprehensions in instance definitions" $ do
      ("regression", "ParallelListCompClass.hs") `shouldGive` [Module "ParallelListCompClass" Nothing []]

    it "works with foreign imports" $ do
      ("regression", "ForeignImport.hs") `shouldGive` [Module "ForeignImport" Nothing []]

    it "works with rewrite rules" $ do
      ("regression", "RewriteRules.hs") `shouldGive` [Module "RewriteRules" Nothing [" doc for foo"]]

    it "strips CR from dos line endings" $ do
      ("dos-line-endings", "Foo.hs") `shouldGive` [Module "Foo" Nothing ["\n foo\n bar\n baz"]]

    it "works with a module that splices in an expression from an other module" $ do
      ("th", "Foo.hs") `shouldGive` [Module "Foo" Nothing [" some documentation"], Module "Bar" Nothing []]

    it "works for type families and GHC 7.6.1" $ do
      ("type-families", "Foo.hs") `shouldGive` [Module "Foo" Nothing []]
