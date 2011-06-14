#!/bin/bash

set -o nounset
set -o errexit

cd "`dirname $0`"

./hunit/runtests.sh
./integration/runtests.sh
./selftest.sh
