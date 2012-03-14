#!/bin/bash

cd "`dirname $0`"

./hunit/runtests.sh
runhaskell -i../src -packageghc Spec.hs
./integration/runtests.sh
./selftest.sh
