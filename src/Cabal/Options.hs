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
  , "--with-repl"
  ]

rejectUnsupportedOptions :: [String] -> IO ()
rejectUnsupportedOptions args = case getOpt' Permute options args of
  (xs, _, _, _) | ListOptions `elem` xs -> do
    let
      names :: [String]
      names = concat [map (\ c -> ['-', c]) short ++ map ("--" <> ) long | Option short long _ _ <- documentedOptions]
    putStr (unlines names)
    exitSuccess
  (_, _, unsupported : _, _) -> do
    die $ "Error: cabal: unrecognized 'doctest' option `" <> unsupported <> "'"
  _ -> pass

data Argument = Argument String (Maybe String) | ListOptions
  deriving (Eq, Show)

options :: [OptDescr Argument]
options =
    Option [] ["list-options"] (NoArg ListOptions) ""
  : documentedOptions

documentedOptions :: [OptDescr Argument]
documentedOptions = map toOptDescr Repl.options
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
        argument value = Argument ("--" <> long) value

discardReplOptions :: [String] -> [String]
discardReplOptions args = case getOpt Permute options args of
  (xs, _, _) -> [renderArgument name value | Argument name value <- xs, Set.notMember name replOnlyOptions]
  where
    renderArgument name = \ case
      Nothing -> name
      Just value -> name <> "=" <> value
