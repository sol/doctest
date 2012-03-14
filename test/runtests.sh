#!/bin/bash

cd "`dirname $0`"

./hunit/runtests.sh
./parse/runtests.sh
./integration/runtests.sh
./selftest.sh
