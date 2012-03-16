#!/bin/bash

cd "`dirname $0`"

DOCTEST="${1:-"../dist/build/doctest/doctest"}"

if [ -x "$DOCTEST" ]; then
    SRCDIR="../src"
    "$DOCTEST" --optghc=-packageghc --optghc=-i$SRCDIR --optghc=-i../dist/build/autogen/ $SRCDIR/Main.hs --optghc=-optP-include --optghc=-optP../dist/build/autogen/cabal_macros.h
else
    echo "$DOCTEST is not executable!"
fi
