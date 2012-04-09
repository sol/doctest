[![Build Status](https://secure.travis-ci.org/sol/doctest-haskell.png)](http://travis-ci.org/sol/doctest-haskell)

About
=====

`doctest` is a small program, that checks
[examples in Haddock comments](http://www.haskell.org/haddock/doc/html/ch03s08.html#id566093).
It is modeled after
[doctest for Python](http://docs.python.org/library/doctest.html).



Installation
============

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

Fore more information, see the [section on paths in the Cabal User Guide]
(http://www.haskell.org/cabal/users-guide/#paths-in-the-simple-build-system).

Usage
=====

Below is a small Haskell module.
The module contains a Haddock comment with some examples of interaction.
The examples demonstrate how the module is supposed to be used.

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

(A comment line starting with `>>>` denotes an _expression_.
All comment lines following an expression denote the _result_ of that expression.
Result is defined by what an
[REPL](http://en.wikipedia.org/wiki/Read-eval-print_loop) (e.g. ghci)
prints to `stdout` and `stderr` when evaluating that expression.)

With `doctest` you may check whether the implementation satisfies the given examples, by typing:

    doctest Fib.hs

You may produce Haddock documentation for that module with:

    haddock -h Fib.hs -o doc/

## Using GHC extensions

The easiest way to tell Doctest about GHC extensions is to use [LANGUAGE
pragmas] [language-pragma] in your source files.

Alternatively you can pass any GHC options to Doctest with `--optghc`.

    doctest --options=-cpp Foo.hs

## Hiding examples from Haddock

You can put examples into [named chunks] [named-chunks], and not refer to them
in the export list.  That way they will not be part of the generated Haddock
documentation, but Doctest will still find them.


Hacking
=======

Submit pull request on GitHub.  Add tests for new features.  Make sure that the
test suite passes with your changes, before submitting any pull requests:

    cabal configure --enable-tests && cabal build && cabal test


Contributors
============

See the [list of contributors](https://github.com/sol/doctest-haskell/contributors).


[named-chunks]: http://www.haskell.org/haddock/doc/html/ch03s05.html
[language-pragma]: http://www.haskell.org/ghc/docs/latest/html/users_guide/pragmas.html#language-pragma
