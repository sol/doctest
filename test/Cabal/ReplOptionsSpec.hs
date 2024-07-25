{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Cabal.ReplOptionsSpec (spec, unsupported) where

import           Imports

import           Test.Hspec

import           Data.List
import           System.Process
import           Data.Set (Set)
import qualified Data.Set as Set

import           Cabal.ReplOptions

phony :: [String]
phony = [
    "with-PROG"
  , "PROG-option"
  , "PROG-options"
  ]

undocumented :: Set String
undocumented = Set.fromList [
    "--enable-optimisation"
  , "--disable-optimisation"
  , "--haddock-hyperlink-sources"
  , "--haddock-hyperlinked-source"
  ]

unsupported :: Set String
unsupported = undocumented <> Set.fromList (map ("--" <>) phony)

spec :: Spec
spec = do
  describe "options" $ do
    it "is the list of documented 'repl' options" $ do
      documentedOptions <- parseOptions <$> readProcess "cabal" ["help", "repl"] ""
      options `shouldBe` filter (optionName >>> (`notElem` phony)) documentedOptions

    it "is consistent with 'cabal repl --list-options'" $ do
      let
        optionNames :: Option -> [String]
        optionNames option = reverse $ "--" <> optionName option : case optionShortName option of
          Nothing -> []
          Just c -> [['-', c]]
  
      repl <- filter (`Set.notMember` unsupported) . lines <$> readProcess "cabal" ["repl", "--list-options"] ""
      concatMap optionNames options `shouldBe` repl

parseOptions :: String -> [Option]
parseOptions = map parseOption . takeOptions
  where
    parseOption :: String -> Option
    parseOption input = case input of
      longAndHelp@('-':'-':_) -> parseLongOption Nothing longAndHelp
      '-':short:',':' ':longAndHelp -> parseLongOption (Just short) longAndHelp
      '-':short:'[':(breakOn ']' ->
        (_arg, ']':',':' ':longAndHelp)) -> parseLongOption (Just short) longAndHelp
      '-':short:' ':(breakOn ' ' ->
        (arg, ' ':'o':'r':' ':(stripPrefix ('-':short:arg) ->
          Just (',':' ':longAndHelp)))) -> parseLongOption (Just short) longAndHelp
      _ -> err
      where
        parseLongOption :: Maybe Char -> String -> Option
        parseLongOption short longAndHelp = case breakOnAny " [=" longAndHelp of
          ('-':'-':long, ' ':help) -> accept long NoArgument help
          ('-':'-':long, '[':'=': (breakOn ']' ->
            (arg, ']':help))) -> accept long (OptionalArgument arg) help
          ('-':'-':long, '=':(breakOn ' ' ->
            (arg, ' ':help))) -> accept long (Argument arg) help
          _ -> err
          where
            accept :: String -> Argument -> String -> Option
            accept long arg help = Option long short arg (strip help)

        err :: HasCallStack => Option
        err = error input

        breakOn c = break (== c)
        breakOnAny xs = break (`elem` xs)

    takeOptions :: String -> [String]
    takeOptions input = map strip . joinLines $ case break (== "Flags for repl:") (lines input) of
      (_, "Flags for repl:" : xs) -> case break (== "") xs of
        (ys, "" : _) -> ys
        _ -> undefined
      _ -> undefined

    joinLines :: [String] -> [String]
    joinLines = go
      where
        go = \ case
          x : y : ys | isOption y  -> x : go (y : ys)
          x : y : ys -> go $ (x ++ ' ' : strip y) : ys
          x : xs -> x : xs
          [] -> []

        isOption = isPrefixOf " -"
