{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

import Data.List (nub)
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import Test.HUnit
import Util (runDoctest)
import HaddockBackend.Api

import System.Environment (getEnv)


main = $(defaultMainGenerator)


ghci = Interaction

-- | Group 'DocTest's by modules.
data ModuleGroup = ModuleGroup {
    sourceFile    :: String
  , moduleName    :: String
  , interactions_ :: [[Interaction]]
} deriving Eq

instance Show ModuleGroup where
  show x = "ModuleGroup " ++ sourceFile x ++ " " ++ moduleName x ++ "\n" ++ formatedInteractions
    where
      formatedInteractions = unlines $ map (unlines . map show) $ interactions_ x


case_parseTestSimple = parseTest "parseTestSimple"  ["Fib.hs"]
  [ ModuleGroup "Fib.hs" "Fib"
    [
      [ ghci "putStrLn \"foo\""
        ["foo"]
      , ghci "putStr \"bar\""
        ["bar"]
      , ghci "putStrLn \"baz\""
        ["baz"]
      ]
    ]
  ]

parseTest workingDir sourceFiles expectedResult = do

  -- get path to doctest binary
  bin <- getEnv "DOCTEST"
  doctestBin <- canonicalizePath bin

  -- We can use Haddock for parsing only once per process.  For this reason we
  -- use the `doctest` binary with the '--dump-only' flag and 'read' the
  -- output.
  r <- runDoctest doctestBin workingDir ("--dump-only" : sourceFiles)
  let result = groupDocTests $ read r
  expectedResult @=? result
  return ()

-- | Group 'DocTest's by origination module.
groupDocTests :: [DocTest] -> [ModuleGroup]
groupDocTests docTests = grouped
  where
    modules :: [(String, String)] -- (sourceFile, moduleName)
    modules = nub $ map (\x -> (source x, module_ x)) docTests

    docTestsByModule :: (String, String) -> [DocTest]
    docTestsByModule m = filter predicate docTests
      where
        predicate x = (source x == fst m) && (module_ x == snd m)

    grouped = map groupByModule modules
      where
        groupByModule :: (String, String) -> ModuleGroup
        groupByModule m = ModuleGroup (fst m) (snd m) (map interactions $ docTestsByModule m)
