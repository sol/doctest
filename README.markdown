What is `doctest`
===============

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


Hacking
=======

`doctest` is still experimental.  Patches are gladly welcome!


Contributors (in order of appearance)
=====================================

Simon Hengel \<simon.hengel@wiktory.org\>

Sakari Jokinen \<sakariij@gmail.com\>
