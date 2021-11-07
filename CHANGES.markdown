Changes in 0.18.2
  - GHC 9.2 compatibility. (#305, thanks to Ryan Scott and Matthew Pickering)

Changes in 0.18.1
  - GHC 9.0 compatibility. (#275)

Changes in 0.18
  - Don't use unqualified references to `stderr` or `stdout` which may collide with definitions in user code. (#201)
  - Remove support for cabal-install sandboxes. They have been obsoleted in practice by Nix-style builds in cabal-install (i.e., the `v2-*` commands) and stack.

Changes in 0.17
  - #266:
    - doctest now annotates its internal marker string as a `String`, to prevent misbehaviour in `OverloadedStrings` environments. This has a theoretical chance of breakage; if you're affected, please open an issue.
    - `evalEcho` no longer preserves `it`.

Changes in 0.16.3
  - Add a cursor to highlight the differing portion between the
    expected and actual output. (#249)
  - GHC 8.10 compatibility. (#247, #257)

Changes in 0.16.2
  - Add doctest's necessary-for-operation options to GHC's command
    line at the end, so that they over-ride anything provided by the
    user. (#233)
  - Allow GHC 8.8.

Changes in 0.16.1
  - Fix loading plugins in doctests. (#224)
  - Require QuickCheck 2.13.1 or newer.
  - Remove dependency on `with-location`

Changes in 0.16.0.1
  - Bump bounds to allow GHC 8.6. (#210)

Changes in 0.16.0
  - Output format has changed to (hopefully) be more machine consumable. (#200)

Changes in 0.15.0
  - Add `--verbose` for printing each test as it is run

Changes in 0.14.1
  - Add test assets to source tarball (see #189)

Changes in 0.14.0
  - GHC 8.4 compatibility.

Changes in 0.13.0
  - Add `--preserve-it` for allowing the `it` variable to be preserved between examples

Changes in 0.12.0
  - Preserve the 'it' variable between examples

Changes in 0.11.4
  - Add `--fast`, which disables running `:reload` between example groups

Changes in 0.11.3
  - Add `--info`
  - Add `--no-magic`

Changes in 0.11.2
  - Make `...` match zero lines

Changes in 0.11.1
 - Fix an issue with Unicode output on Windows (see #149)

Changes in 0.11.0
 - Support for GHC 8.0.1-rc2

Changes in 0.10.1
 - Automatically expand directories into contained Haskell source files (thanks @snoyberg)
 - Add cabal_macros.h and autogen dir by default (thanks @snoyberg)

Changes in 0.10.0
 - Support HASKELL_PACKAGE_SANDBOXES (thanks @snoyberg)

Changes in 0.9.13
 - Add ellipsis as wildcard

Changes in 0.9.12
 - Add support for GHC 7.10

Changes in 0.9.11
 - Defaults ambiguous type variables to Integer (#74)

Changes in 0.9.10
 - Add support for the upcoming GHC 7.8 release

Changes in 0.9.9
 - Add support for multi-line statements

Changes in 0.9.8
 - Support for GHC HEAD (7.7)

Changes in 0.9.7
 - Ignore trailing whitespace when matching example output

Changes in 0.9.6
 - Fail gracefully if GHCi is not supported (#46)

Changes in 0.9.5
 - Fix a GHC panic with GHC 7.6.1 (#41)

Changes in 0.9.4
 - Respect HASKELL_PACKAGE_SANDBOX (#39)
 - Print path to ghc on --version

Changes in 0.9.3
 - Properly handle additional object files (#38)

Changes in 0.9.2
 - Add support for QuickCheck properties

Changes in 0.9.1
 - Fix an issue with GHC 7.6.1 and type families

Changes in 0.9.0
 - Add support for setup code (see README).
 - There is no distinction between example/interaction anymore.  Each
   expression is counted as an example in the summary.

Changes in 0.8.0
 - Doctest now directly accepts arbitrary GHC options, prefixing GHC options
   with --optghc is no longer necessary

Changes in 0.7.0
 - Print source location for failing tests
 - Output less clutter on failing examples
 - Expose Doctest's functionality through a very simplistic API, which can be
   used for cabal integration

Changes in 0.6.1
 - Fix a parser bug with CR+LF line endings

Changes in 0.6.0
 - Support for ghc-7.4
 - Doctest now comes with it's own parser and does not depend on Haddock
   anymore

Changes in 0.5.2
 - Proper handling of singular/plural when printing stats
 - Improve handling of invalid command line options

Changes in 0.5.1
 - Adapted for ghc-7.2

Changes in 0.5.0
 - Print number of interactions to stderr before running tests
 - Exit with exitFailure on failed tests
 - Improve documentation
 - Give a useful error message if ghc is not executable
