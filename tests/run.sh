#!/bin/bash

set -o nounset
set -o errexit

cd "`dirname $0`"

./run_interpreter_tests.sh
./runtests.sh
./selftest.sh
