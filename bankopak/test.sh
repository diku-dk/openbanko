#!/usr/bin/env bash

set -euo pipefail

echo "Testing algorithm $1"
cat $2 | \
    ./bankopak -c $1 | ./bankopak -d $1 | \
    ../bankopladeformat/bankocmp /dev/stdin $2; \
