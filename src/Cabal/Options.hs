{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Cabal.Options (
  rejectUnsupportedOptions
, discardReplOptions

#ifdef TEST
, Option(..)
, pathOptions
, replOptions
, shouldReject
, Discard(..)
, shouldDiscard
#endif
) where

import           Imports

import           Data.List
import           System.Exit

import           Data.Set (Set)
import qualified Data.Set as Set

data Option = Option {
  optionName :: String
, _optionArgument :: OptionArgument
}

data OptionArgument = Argument | NoArgument

pathOptions :: [Option]
pathOptions = [
    Option "-z" NoArgument
  , Option "--ignore-project" NoArgument
  , Option "--output-format" Argument
  , Option "--compiler-info" NoArgument
  , Option "--cache-home" NoArgument
  , Option "--remote-repo-cache" NoArgument
  , Option "--logs-dir" NoArgument
  , Option "--store-dir" NoArgument
  , Option "--config-file" NoArgument
  , Option "--installdir" NoArgument
  ]

replOptions :: [Option]
replOptions = [
    Option "-z" NoArgument
  , Option "--ignore-project" NoArgument
  , Option "--repl-no-load" NoArgument
  , Option "--repl-options" Argument
  , Option "--repl-multi-file" Argument
  , Option "-b" Argument
  , Option "--build-depends" Argument
  , Option "--no-transitive-deps" NoArgument
  , Option "--enable-multi-repl" NoArgument
  , Option "--disable-multi-repl" NoArgument
  , Option "--keep-temp-files" NoArgument
  ]

rejectUnsupportedOptions :: [String] -> IO ()
rejectUnsupportedOptions = mapM_ $ \ arg -> when (shouldReject arg) $ do
  die "Error: cabal: unrecognized 'doctest' option `--installdir'"

shouldReject :: String -> Bool
shouldReject arg =
     Set.member arg rejectNames
  || (`any` longOptionsWithArgument) (`isPrefixOf` arg)
  where
    rejectNames :: Set String
    rejectNames = Set.fromList (map optionName pathOptions)

    longOptionsWithArgument :: [String]
    longOptionsWithArgument = [name <> "=" | Option name@('-':'-':_) Argument <- pathOptions]

discardReplOptions :: [String] -> [String]
discardReplOptions = go
  where
    go = \ case
      [] -> []
      arg : args -> case shouldDiscard arg of
        Keep -> arg : go args
        Discard -> go args
        DiscardWithArgument -> go (drop 1 args)

data Discard = Keep | Discard | DiscardWithArgument
  deriving (Eq, Show)

shouldDiscard :: String -> Discard
shouldDiscard arg
  | Set.member arg flags = Discard
  | Set.member arg options = DiscardWithArgument
  | isOptionWithArgument = Discard
  | otherwise = Keep
  where
    flags :: Set String
    flags = Set.fromList [name | Option name NoArgument <- replOptions]

    options :: Set String
    options = Set.fromList (longOptions <> shortOptions)

    longOptions :: [String]
    longOptions = [name | Option name@('-':'-':_) Argument <- replOptions]

    shortOptions :: [String]
    shortOptions = [name | Option name@['-', _] Argument <- replOptions]

    isOptionWithArgument :: Bool
    isOptionWithArgument = any (`isPrefixOf` arg) (map (<> "=") longOptions <> shortOptions)
