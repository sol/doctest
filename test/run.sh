#!/bin/bash

set -o nounset
set -o errexit

cd "`dirname $0`/.."

runhaskell -isrc -itest -packageghc test/Spec.hs
./test/integration/runtests.sh
./test/selftest.sh
