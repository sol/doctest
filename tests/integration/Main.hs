module Main where

import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import Test.HUnit (Test(TestList), runTestTT)
import Util

main :: IO ()
main = do
  -- get path to doctest binary
  [bin] <- getArgs
  bin' <- canonicalizePath bin
  _ <- runTestTT $ tests $ doctestTestCase bin'
  return ();
  where
    tests doctest = TestList [

    -- Tests
    -- =====

    --  * testImport
        doctest "testImport" ["ModuleA.hs"]
        (cases 2)
      , doctest ".." ["--optghc=-iintegration/testImport", "integration/testImport/ModuleA.hs"]
        (cases 2)

    --  * testCommentLocation
      , doctest "." ["testCommentLocation/Foo.hs"]
        (cases 8)

    -- * testPutStr
      , doctest "testPutStr" ["Fib.hs"]
        (cases 1)

    -- * testFailOnMultiline
      , doctest "testFailOnMultiline" ["Fib.hs"]
        (cases 1) {errors = 1}

    -- * testNotInScope
      , doctest "testNotInScope" ["Fib.hs"]
        (cases 1)

    -- * testBlankline
      , doctest "testBlankline" ["Fib.hs"]
        (cases 1)

    -- Bugfix tests
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

    -- * bugfixImportHierarchical
      , doctest "bugfixImportHierarchical" ["ModuleA.hs", "ModuleB.hs"]
        (cases 2)

    -- * bugfixMultipleModules
      , doctest "bugfixMultipleModules" ["ModuleA.hs"]
        (cases 3)

    -- Open bugs
    -- =========

    {-
    -- * bugFoo
      , doctest "bugFoo" ["Foo.hs"]
        (cases 3) {errors = 0, failures = 1}
        -- expected: (cases 3)
    -}
      ]
