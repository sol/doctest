#!/bin/bash

cd "`dirname $0`"

DOCTEST="${1:-"../../dist/build/doctest/doctest"}"

if [ -x "$DOCTEST" ]; then
    DOCTEST=$DOCTEST runhaskell -hide-all-packages \
               -packagebase \
               -packageHUnit \
               -packageprocess \
               -packagedirectory \
               -packagecontainers \
               -packageghc \
               -packagehaddock \
               -packagetest-framework \
               -packagetest-framework-hunit \
               -packagetest-framework-th \
               -i../../src \
                Main.hs
else
    echo "$DOCTEST is not executable!"
fi
