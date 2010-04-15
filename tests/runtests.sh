#!/bin/bash

cd "`dirname $0`"

DOCTEST="${1:-"../dist/build/doctest/doctest"}"

if [ -x "$DOCTEST" ]; then
    runhaskell -hide-all-packages \
               -packagebase \
               -packageMissingH \
               -packageHUnit \
               -packageprocess \
               -packagedirectory \
                Main.hs "$DOCTEST"
else
    echo "$DOCTEST is not executable!"
fi
