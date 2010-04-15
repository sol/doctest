#!/bin/bash

cd "`dirname $0`"

DOCTEST="${1:-"../dist/build/doctest/doctest"}"

if [ -x "$DOCTEST" ]; then
    SRCDIR="../src"

    # guess number of cases..
    echo Guess: `grep -r "ghci>" ../src/ | wc -l`

    "$DOCTEST" -w --optghc=-i$SRCDIR $SRCDIR/Main.hs
else
    echo "$DOCTEST is not executable!"
fi
