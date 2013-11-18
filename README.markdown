# Doctest: Test interactive Haskell examples

`doctest` is a small program, that checks [examples in Haddock comments]
(http://www.haskell.org/haddock/doc/html/ch03s08.html#id566093).  It is similar
to the [popular Python module with the same name]
(http://docs.python.org/library/doctest.html).


## Installation

`doctest` is available from
[Hackage](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/doctest).
Install it, by typing:

    cabal install doctest

Make sure that Cabal's `bindir` is on your `PATH`.

On Linux:

    export PATH="$HOME/.cabal/bin:$PATH"

On Mac OS X:

    export PATH="$HOME/Library/Haskell/bin:$PATH"

On Windows it's `C:\Documents And Settings\user\Application Data\cabal\bin`.

For more information, see the [section on paths in the Cabal User Guide]
(http://www.haskell.org/cabal/users-guide/installing-packages.html#paths-in-the-simple-build-system).

## Usage

Below is a small Haskell module.
The module contains a Haddock comment with some examples of interaction.
The examples demonstrate how the module is supposed to be used.

```haskell
module Fib where

-- | Compute Fibonacci numbers
--
-- Examples:
--
-- >>> fib 10
-- 55
--
-- >>> fib 5
-- 5
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

(A comment line starting with `>>>` denotes an _expression_.
All comment lines following an expression denote the _result_ of that expression.
Result is defined by what an
[REPL](http://en.wikipedia.org/wiki/Read-eval-print_loop) (e.g. ghci)
prints to `stdout` and `stderr` when evaluating that expression.)

With `doctest` you may check whether the implementation satisfies the given examples, by typing:

    doctest Fib.hs

You may produce Haddock documentation for that module with:

    haddock -h Fib.hs -o doc/

### Example groups

Examples from a single Haddock comment are grouped together and share the same
scope.  E.g. the following works:

```haskell
-- |
-- >>> let x = 23
-- >>> x + 42
-- 65
```

If an example fails, subsequent examples from the same group are skipped.  E.g.
for

```haskell
-- |
-- >>> let x = 23
-- >>> let n = x + y
-- >>> print n
```

`print n` is not tried, because `let n = x + y` fails (`y` is not in scope!).

### Setup code

You can put setup code in a [named chunk][named-chunks] with the name `$setup`.
The setup code is run before each example group.  If the setup code produces
any errors/failures, all tests from that module are skipped.

Here is an example:

```haskell
module Foo where
-- $setup
-- >>> let x = 23

-- |
-- >>> foo + x
-- 65
foo :: Int
foo = 42
```


### Multi-line input
GHCi supports commands which span multiple lines, and the same syntax works for doctest:

```haskell
-- |
-- >>> :{
--  let
--    x = 1
--    y = 2
--  in x + y + multiline
-- :}
-- 6
multiline = 3
```

Note that `>>>` can be left of for the lines following the first: this so that
haddock does not strip leading whitespace. The expected output has whitespace
stripped relative to the :}.

Some peculiarities on the ghci side mean that whitespace at the very start is lost.
This breaks the example `broken`, since the the x and y are aligned from ghci's
perspective.  A workaround is to avoid leading space, or add a newline such
that the indentation does not matter:

```haskell
{- | >>> {:
let x = 1
    y = 2
  in x + y + works
:}
3
-}
works = 3

{- | >>> {:
 let x = 1
     y = 2
  in x + y + broken
:}
3
-}
broken = 3
```

### QuickCheck properties

Haddock (since version 2.13.0) has markup support for properties.  Doctest can
verify properties with QuickCheck.  A simple property looks like this:

```haskell
-- |
-- prop> \xs -> sort xs == (sort . sort) (xs :: [Int])
```

The lambda abstraction is optional and can be omitted:

```haskell
-- |
-- prop> sort xs == (sort . sort) (xs :: [Int])
```

A complete example that uses setup code is below:

```haskell
module Fib where

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> newtype Small = Small Int deriving Show
-- >>> instance Arbitrary Small where arbitrary = Small . (`mod` 10) <$> arbitrary

-- | Compute Fibonacci numbers
--
-- The following property holds:
--
-- prop> \(Small n) -> fib n == fib (n + 2) - fib (n + 1)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

### Hiding examples from Haddock

You can put examples into [named chunks] [named-chunks], and not refer to them
in the export list.  That way they will not be part of the generated Haddock
documentation, but Doctest will still find them.

[named-chunks]: http://www.haskell.org/haddock/doc/html/ch03s05.html

### Using GHC extensions

The easiest way to tell Doctest about GHC extensions is to use [LANGUAGE
pragmas] [language-pragma] in your source files.

Alternatively you can pass any GHC options to Doctest, e.g.:

    doctest -cpp Foo.hs

[language-pragma]: http://www.haskell.org/ghc/docs/latest/html/users_guide/pragmas.html#language-pragma

### Cabal integration

Doctest provides both, an executable and a library.  The library exposes a
function `doctest` of type:

```haskell
doctest :: [String] -> IO ()
```

Doctest's own `main` is simply:

```haskell
main = getArgs >>= doctest
```

Consequently, it is possible to create a custom executable for a project, by
passing all command-line arguments that are required for that project to
`doctest`.  A simple example looks like this:

```haskell
-- file doctests.hs
import Test.DocTest
main = doctest ["-isrc", "src/Main.hs"]
```

And a corresponding Cabal test suite section like this:

    test-suite doctests
      type:          exitcode-stdio-1.0
      ghc-options:   -threaded
      main-is:       doctests.hs
      build-depends: base, doctest >= 0.8

## Development [![Build Status](https://secure.travis-ci.org/sol/doctest-haskell.png)](http://travis-ci.org/sol/doctest-haskell)

Join in at `#hspec` on freenode.

Discuss your ideas first, ideally by opening an issue on GitHub.

Add tests for new features, and make sure that the test suite passes with your
changes.

    cabal configure --enable-tests && cabal build && cabal test


## Contributors

 * Adam Vogt
 * Ankit Ahuja
 * Edward Kmett
 * Hiroki Hattori
 * Joachim Breitner
 * Kazu Yamamoto
 * Levent Erkok
 * Matvey Aksenov
 * Michael Snoyman
 * Sakari Jokinen
 * Simon Hengel
