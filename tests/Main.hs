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
        (cases 2)
      , doctest ".." ["--optghc=-itests/testImport", "tests/testImport/ModuleA.hs"]
        (cases 2)

    --  * testCommentLocation
      , doctest "." ["testCommentLocation/Foo.hs"]
        (cases 11)

    -- * testPutStr
      , doctest "testPutStr" ["Fib.hs"]
        (cases 2)

    -- Bugfix Tests
    -- ============

    --  * bugfixWorkingDirectory
      , doctest "bugfixWorkingDirectory" ["Fib.hs"]
        (cases 1)
      , doctest "bugfixWorkingDirectory" ["examples/Fib.hs"]
        (cases 2)

    -- * bugfixOutputToStdErr
      , doctest "bugfixOutputToStdErr" ["Fib.hs"]
        (cases 1)

    -- * bugfixMultipleStatements
      , doctest "bugfixMultipleStatements" ["Fib.hs"]
        (cases 1)

    -- open bugs
    -- =========

    -- * bugImportHierarchical
      , doctest "bugImportHierarchical" ["ModuleA.hs", "ModuleB.hs"]
        (cases 2) {errors = 1, failures = 1}
      ]
