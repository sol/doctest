# Doctest: Test interactive Haskell examples

`doctest` is a tool that checks
[examples](https://haskell-haddock.readthedocs.io/latest/markup.html#examples)
and
[properties](https://haskell-haddock.readthedocs.io/latest/markup.html#properties)
in Haddock comments.
It is similar in spirit to the [popular Python module with the same name](https://docs.python.org/3/library/doctest.html).

* [Getting started](#getting-started)
   * [Installation](#installation)
   * [A basic example](#a-basic-example)
* [Running doctest for a Cabal package](#running-doctest-for-a-cabal-package)
   * [Passing doctest options to cabal repl](#passing-doctest-options-to-cabal-repl)
   * [Cabal integration](#cabal-integration)
* [Writing examples and properties](#writing-examples-and-properties)
   * [Example groups](#example-groups)
      * [A note on performance](#a-note-on-performance)
   * [Setup code](#setup-code)
   * [Multi-line input](#multi-line-input)
   * [Multi-line output](#multi-line-output)
   * [Matching arbitrary output](#matching-arbitrary-output)
   * [QuickCheck properties](#quickcheck-properties)
   * [Hiding examples from Haddock](#hiding-examples-from-haddock)
   * [Using GHC extensions](#using-ghc-extensions)
* [Limitations](#limitations)
* [Doctest in the wild](#doctest-in-the-wild)
* [Development](#development)
* [Contributors](#contributors)


# Getting started

## Installation

`doctest` is available from
[Hackage](https://hackage.haskell.org/package/doctest).
Install it with:

    cabal update && cabal install --ignore-project doctest

Make sure that Cabal's `installdir` is on your `PATH`.

On Linux / macOS / BSD:

```bash
 # requires cabal-install 3.12, or later
export PATH="$(cabal -v0 path --installdir):$PATH"
```
or
```bash
export PATH="$HOME/.local/bin:$PATH"
```

On Windows with PowerShell:

```pwsh
# requires cabal-install 3.12, or later
$Env:PATH = "$(cabal -v0 path --installdir)" + ";" + $Env:PATH
```
or
```batch
set PATH="%AppData%\cabal\bin\;%PATH%"
```
## A basic example

Below is a small Haskell module.
The module contains a Haddock comment with some examples of interaction.
The examples demonstrate how the module is supposed to be used.

```haskell
-- src/Fib.hs
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
Result is defined by what a
[REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) (e.g. ghci)
prints to `stdout` and `stderr` when evaluating that expression.)

With `doctest` you can check whether the implementation satisfies the given
examples:

```
doctest src/Fib.hs
```


# Running `doctest` for a Cabal package

The easiest way to run `doctest` for a Cabal package is via `cabal repl --with-compiler=doctest`.

This doesn't make a big difference for a simple package, but in more involved
situations `cabal` will make sure that all dependencies are available and it
will pass any required GHC options to `doctest`.

A simple `.cabal` file for `Fib` looks like this:

```cabal
-- fib.cabal
cabal-version: 1.12

name:           fib
version:        0.0.0
build-type:     Simple

library
  build-depends: base == 4.*
  hs-source-dirs: src
  exposed-modules: Fib
  default-language: Haskell2010

```

With a `.cabal` file in place, it is possible to run `doctest` via `cabal repl`:

```bash
$ cabal repl --with-compiler=doctest
...
Examples: 2  Tried: 2  Errors: 0  Failures: 0
```


Notes:

- If you use properties you need to pass `--build-depends=QuickCheck` and
  `--build-depends=template-haskell` to `cabal repl`.

- You likely want to reset the warning strategy for `cabal repl` with
  `--repl-options='-w -Wdefault'`.

- `doctest` always uses the version of GHC it was compiled with.  Reinstalling
  `doctest` with `cabal install doctest --overwrite-policy=always` before each
  invocation ensures that it uses the same version of GHC as is on the `PATH`.

- Technically, `cabal build` is not necessary. `cabal repl --with-compiler=doctest`
  will build any dependencies as needed.  However, it's more robust to run
  `cabal build` first (specifically it is not a good idea to build
  `ghc-paths` with `--with-compiler=doctest`).

So a more robust way to call `doctest` is as follows:

```
cabal install doctest --ignore-project --overwrite-policy=always && cabal build && cabal repl --build-depends=QuickCheck --build-depends=template-haskell --with-compiler=doctest --repl-options='-w -Wdefault'
```

(This is what you want to use on CI.)

## Passing `doctest` options to `cabal repl`

You can pass `doctest` options like `--fast`, `--preserve-it` and `--verbose` to
`cabal repl` via `--repl-options`.

Example:

```bash
$ cabal repl --with-compiler=doctest --repl-options=--verbose
### Started execution at src/Fib.hs:7.
### example:
fib 10
### Successful!

### Started execution at src/Fib.hs:10.
### example:
fib 5
### Successful!

# Final summary:
Examples: 2  Tried: 2  Errors: 0  Failures: 0
```

## Cabal integration

***NOTE:*** This feature is experimental.

***NOTE:*** This feature requires `cabal-install` version 3.12 or later.


```bash
$ cabal install --ignore-project doctest --flag cabal-doctest
```

```bash
$ cabal doctest
Examples: 2  Tried: 2  Errors: 0  Failures: 0
```

# Writing examples and properties

## Example groups

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

`print n` is skipped, because `let n = x + y` fails (as `y` is not in scope).

### A note on performance

By default, `doctest` calls `:reload` between each group to clear GHCi's scope
of any local definitions. This ensures that previous examples cannot influence
later ones. However, it can lead to performance penalties if you are using
`doctest` in a project with many modules. One possible remedy is to pass the
`--fast` flag to `doctest`, which disables calling `:reload` between groups.
If `doctest`s are running too slowly, you might consider using `--fast`.
(With the caveat that the order in which groups appear now matters!)

However, note that due to a
[bug on GHC 8.2.1 or later](https://gitlab.haskell.org/ghc/ghc/-/issues/14052),
the performance of `--fast` suffers significantly when combined with the
`--preserve-it` flag (which keeps the value of GHCi's `it` value between
examples).

## Setup code

You can put setup code in a [named chunk][named-chunks] with the name `$setup`.
The setup code is run before each example group.  If the setup code produces
any errors/failures, all tests from that module are skipped.

Here is an example:

```haskell
module Foo where

import Bar.Baz

-- $setup
-- >>> let x = 23 :: Int

-- |
-- >>> foo + x
-- 65
foo :: Int
foo = 42
```

Note that you should not place setup code inbetween the module header (`module
...  where`) and import declarations. GHC will not be able to parse it ([issue
 #167](https://github.com/sol/doctest/issues/167)). It is best to place setup
code right after import declarations, but due to its declarative nature you can
place it anywhere inbetween top level declarations as well.


## Multi-line input
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

Note that `>>>` can be left off for the lines following the first: this is so that
haddock does not strip leading whitespace. The expected output has whitespace
stripped relative to the :}.

Some peculiarities on the ghci side mean that whitespace at the very start is lost.
This breaks the example `broken`, since the x and y aren't aligned from ghci's
perspective.  A workaround is to avoid leading space, or add a newline such
that the indentation does not matter:

```haskell
{- | >>> :{
let x = 1
    y = 2
  in x + y + works
:}
6
-}
works = 3

{- | >>> :{
 let x = 1
     y = 2
  in x + y + broken
:}
3
-}
broken = 3
```

## Multi-line output
If there are no blank lines in the output, multiple lines are handled
automatically.

```haskell
-- | >>> putStr "Hello\nWorld!"
-- Hello
-- World!
```

If however the output contains blank lines, they must be noted
explicitly with `<BLANKLINE>`. For example,

```haskell
import Data.List ( intercalate )

-- | Double-space a paragraph.
--
--   Examples:
--
--   >>> let s1 = "\"Every one of whom?\""
--   >>> let s2 = "\"Every one of whom do you think?\""
--   >>> let s3 = "\"I haven't any idea.\""
--   >>> let paragraph = unlines [s1,s2,s3]
--   >>> putStrLn $ doubleSpace paragraph
--   "Every one of whom?"
--   <BLANKLINE>
--   "Every one of whom do you think?"
--   <BLANKLINE>
--   "I haven't any idea."
--
doubleSpace :: String -> String
doubleSpace = (intercalate "\n\n") . lines
```

## Matching arbitrary output
Any lines containing only three dots (`...`) will match one or more lines with
arbitrary content. For instance,

```haskell
-- |
-- >>> putStrLn "foo\nbar\nbaz"
-- foo
-- ...
-- baz
```

If a line contains three dots and additional content, the three dots will match
anything *within that line*:

```haskell
-- |
-- >>> putStrLn "foo bar baz"
-- foo ... baz
```

## QuickCheck properties

Haddock has markup support for properties.  Doctest can verify properties with
QuickCheck.  A simple property looks like this:

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

If you see an error like the following, ensure that
[QuickCheck](https://hackage.haskell.org/package/QuickCheck) is visible to
`doctest` (e.g. by passing `--build-depends=QuickCheck` to `cabal repl`).

```haskell
<interactive>:39:3:
    Not in scope: ‘polyQuickCheck’
    In the splice: $(polyQuickCheck (mkName "doctest_prop"))

<interactive>:39:3:
    GHC stage restriction:
      ‘polyQuickCheck’ is used in a top-level splice or annotation,
      and must be imported, not defined locally
    In the expression: polyQuickCheck (mkName "doctest_prop")
    In the splice: $(polyQuickCheck (mkName "doctest_prop"))
```

## Hiding examples from Haddock

You can put examples into [named chunks][named-chunks], and not refer to them
in the export list.  That way they will not be part of the generated Haddock
documentation, but Doctest will still find them.

```haskell
-- $
-- >>> 1 + 1
-- 2
```

[named-chunks]: https://haskell-haddock.readthedocs.io/latest/markup.html#named-chunks

## Using GHC extensions

There's two sets of GHC extensions involved when running Doctest:

1. The set of GHC extensions that are active when compiling the module code
   (excluding the doctest examples). The easiest way to specify these
   extensions is through [LANGUAGE pragmas][language-pragma] in your source
   files.

1. The set of GHC extensions that are active when executing the Doctest
   examples. (These are not influenced by the LANGUAGE pragmas in the file.)
   The recommended way to enable extensions for Doctest examples is to switch
   them on like this:

```haskell
-- |
-- >>> :seti -XTupleSections
-- >>> fst' $ (1,) 2
-- 1
fst' :: (a, b) -> a
fst' = fst
```

Alternatively you can pass any GHC options to Doctest, e.g.:

    doctest -XCPP Foo.hs

These options will affect both the loading of the module and the execution of
the Doctest examples.

If you want to omit the information which language extensions are enabled from
the Doctest examples you can use the method described in [Hiding examples from
Haddock](#hiding-examples-from-haddock), e.g.:

```haskell
-- $
-- >>> :seti -XTupleSections
```

[language-pragma]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#language-pragma

# Limitations


- Doctests only works on platforms that have support for GHC's `--interactive` mode (`ghci`).


- Due to [a GHC bug](https://gitlab.haskell.org/ghc/ghc/-/issues/20670), running
  `:set -XTemplateHaskell` within `ghci` may unload any modules that were
  specified on the command-line.

  To address this `doctest >= 0.19.0` does two things:

  1. Doctest always enables `-XTemplateHaskell`.  So it is safe to use Template
    Haskell in examples without enabling the extension explicitly.
  1. Doctest filters out `-XTemplateHaskell` from single-line `:set`-statements.
    So it is still safe to include `:set -XTemplateHaskell` in examples for
    documentation purposes.  It may just not work as intended in `ghci` due to
    that GHC bug.

  Doctest does not filter out `-XTemplateHaskell` from multi-line
  `:set`-statements.  So if you e.g. use

  ```
  >>> :{
  :set -XTemplateHaskell
  :}
  ```
  then you are on your own.

  Note that all platforms that support `--interactive` also support
  `-XTemplateHaskell`.  So this approach does not reduce Doctest's platform
  support.

- Modules that are rejected by `haddock` will not work with `doctest`.  This
  can mean that `doctest` fails on input that is accepted by GHC (e.g.
  [#251](https://github.com/sol/doctest/issues/251)).

- Doctest works best with UTF-8.  If your locale is e.g. `LC_ALL=C`, you may
  want to invoke `doctest` with `LC_ALL=C.UTF-8`.

# Doctest in the wild

You can find real world examples of `Doctest` being used below:

  * [base Data/Maybe.hs](https://github.com/ghc/ghc/blob/669cbef03c220de43b0f88f2b2238bf3c02ed64c/libraries/base/Data/Maybe.hs#L36-L79)
  * [base Data/Functor.hs](https://github.com/ghc/ghc/blob/669cbef03c220de43b0f88f2b2238bf3c02ed64c/libraries/base/Data/Functor.hs#L34-L64)


# Development

Discuss your ideas first, ideally by opening an issue on GitHub.

Add tests for new features, and make sure that the test suite passes with your
changes.

    cabal build && cabal exec $(cabal list-bin spec)


# Contributors

 * Simon Hengel
 * quasicomputational
 * Kazu Yamamoto
 * Andreas Abel
 * Michael Snoyman
 * Michael Orlitzky
 * Sakari Jokinen
 * Adam Vogt
 * Ryan Scott
 * Oleg Grenrus
 * Sönke Hahn
 * Edward Kmett
 * Elliot Marsden
 * Greg Pfeil
 * Ignat Insarov
 * Julian K. Arni
 * Takano Akio
 * Joachim Breitner
 * Alan Zimmerman
 * Alexander Bernauer
 * Alexandre Esteves
 * Anders Persson
 * Ankit Ahuja
 * Artyom Kazak
 * Gabor Greif
 * Guillaume Bouchard
 * Hiroki Hattori
 * Jens Petersen
 * John Chee
 * João Cristóvão
 * Leon Schoorl
 * Levent Erkok
 * Luke Murphy
 * Matvey Aksenov
 * Mitchell Rosen
 * Nick Smallbone
 * Nikos Baxevanis
 * Tamar Christina
 * Veronika Romashkina

For up-to-date list, query

    git shortlog -s
