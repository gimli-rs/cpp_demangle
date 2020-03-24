#!/bin/bash

set -e

cargo build
if [ "$1" == "debug" ]; then
    (cd test && make debug && ./test.sh)
else
    (cd test && make && ./test.sh)
fi
