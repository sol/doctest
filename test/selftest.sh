#!/bin/bash

cd "`dirname $0`"

DOCTEST="${1:-"../dist/build/doctest/doctest"}"

if [ -x "$DOCTEST" ]; then
    SRCDIR="../src"
    "$DOCTEST" --optghc=-packageghc --optghc=-i$SRCDIR $SRCDIR/Main.hs
else
    echo "$DOCTEST is not executable!"
fi
