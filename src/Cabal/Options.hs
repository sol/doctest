{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Cabal.Options (
  rejectUnsupportedOptions
, discardReplOptions

#ifdef TEST
, replOnlyOptions
#endif
) where

import           Imports

import           System.Exit
import           System.Console.GetOpt

import           Data.Set (Set)
import qualified Data.Set as Set

import qualified Cabal.ReplOptions as Repl

replOnlyOptions :: Set String
replOnlyOptions = Set.fromList [
    "-z"
  , "--ignore-project"
  , "--repl-no-load"
  , "--repl-options"
  , "--repl-multi-file"
  , "-b"
  , "--build-depends"
  , "--no-transitive-deps"
  , "--enable-multi-repl"
  , "--disable-multi-repl"
  , "--keep-temp-files"
  ]

rejectUnsupportedOptions :: [String] -> IO ()
rejectUnsupportedOptions args = case getOpt' Permute options args of
  (_, _, unsupported : _, _) -> die $ "Error: cabal: unrecognized 'doctest' option `" <> unsupported <> "'"
  _ -> pass

data Argument = Argument {
  argumentName :: String
, argumentValue :: Maybe String
}

options :: [OptDescr Argument]
options = map toOptDescr Repl.options
  where
    toOptDescr :: Repl.Option -> OptDescr Argument
    toOptDescr (Repl.Option long short arg help) = Option (maybeToList short) [long] (toArgDescr long arg) help

    toArgDescr :: String -> Repl.Argument -> ArgDescr Argument
    toArgDescr long = \ case
      Repl.Argument name -> ReqArg (argument . Just) name
      Repl.NoArgument -> NoArg (argument Nothing)
      Repl.OptionalArgument name -> OptArg argument name
      where
        argument :: Maybe String -> Argument
        argument argumentValue = Argument {
          argumentName = "--" <> long
        , argumentValue
        }

discardReplOptions :: [String] -> [String]
discardReplOptions args = case getOpt Permute options args of
  (xs, _, _) -> concatMap values (filter (not . isReplOnlyOption) xs)
  where
    isReplOnlyOption :: Argument -> Bool
    isReplOnlyOption arg = Set.member (argumentName arg) replOnlyOptions

    values :: Argument -> [String]
    values (Argument name value) = name : maybeToList value
