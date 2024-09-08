{-# LANGUAGE LambdaCase #-}
module Imports (module Imports) where

import           Prelude as Imports
import           Data.Monoid as Imports
import           Data.Maybe as Imports
import           Control.Monad as Imports hiding (forM_)
import           Control.Exception as Imports
import           Data.Foldable as Imports (forM_)
import           Control.Arrow as Imports

import           Data.Char
import           System.Exit
import           System.Process

pass :: Monad m => m ()
pass = return ()

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

call :: FilePath -> [FilePath] -> IO ()
call name args = rawSystem name args >>= \ case
  ExitSuccess -> pass
  err -> exitWith err

exec :: FilePath -> [FilePath] -> IO ()
exec name args = rawSystem name args >>= exitWith
