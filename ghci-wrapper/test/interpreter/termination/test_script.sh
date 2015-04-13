#!/bin/bash

set -o errexit

cd `dirname $0`

mkdir -p build

pkgconf='../../../.cabal-sandbox/*-packages.conf.d'
if [ -d $pkgconf ]; then
  ghc -v0 --make -i../../../src -threaded -odir build -hidir build -package-db $pkgconf test_program.hs
else
  ghc -v0 --make -i../../../src -threaded -odir build -hidir build test_program.hs
fi

./test_program > /dev/null &

PID=$!
sleep 0.5
kill -INT $PID
sleep 1

if kill -0 $PID 2> /dev/null ; then
    # process is still here..
    echo "$PID is still running!"
    echo "fail"
else
    echo "success"
fi

rm -r build
rm test_program
