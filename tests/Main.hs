module Main where

import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import Test.HUnit (Test(TestList), runTestTT)
import TestUtil

main :: IO ()
main = do
  -- get path to doctest binary
  [bin] <- getArgs
  bin' <- canonicalizePath bin
  _ <- runTestTT $ tests $ doctestTestCase bin'
  return ();
  where
    tests doctest = TestList [
    -- dummy
        doctest "." [] (cases 0)

    -- Tests
    -- =====

    --  * testImport
      , doctest "testImport" ["ModuleA.hs"]
        (cases 3)
      , doctest ".." ["--optghc=-itests/testImport", "tests/testImport/ModuleA.hs"]
        (cases 3)

    --  * testCommentLocation
      , doctest "." ["testCommentLocation/Foo.hs"]
        (cases 11)

    -- * testPutStr
      , doctest "testPutStr" ["Fib.hs"]
        (cases 3)

    -- Bugfix Tests
    -- ============

    --  * bugfixWorkingDirectory
      , doctest "bugfixWorkingDirectory" ["Fib.hs"]
        (cases 1)
      , doctest "bugfixWorkingDirectory" ["examples/Fib.hs"]
        (cases 2)

    -- open bugs
    -- =========

    -- * bugImportHierarchical
      , doctest "bugImportHierarchical" ["ModuleA.hs", "ModuleB.hs"]
        (cases 3) {errors = 2, failures = 1}

    -- * bugMultipleStatements
      , doctest "bugMultipleStatements" ["Fib.hs"]
        (cases 2) {failures = 1}

    -- * bugOutputToStdErr
      , doctest "bugOutputToStdErr" ["Fib.hs"]
        (cases 2) {failures = 1}
      ]
