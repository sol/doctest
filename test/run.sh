#!/bin/bash

set -o nounset
set -o errexit

cd "`dirname $0`"

runhaskell -i../src -packageghc Spec.hs
./integration/runtests.sh
./selftest.sh
