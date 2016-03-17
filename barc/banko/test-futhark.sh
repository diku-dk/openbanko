#!/bin/bash

set -e
cd "$(dirname "$0")"

expected='[True, True, False, False, False]'

cat tests/plade-rigtig-0.bankopladeformat \
    tests/plade-rigtig-1.bankopladeformat \
    tests/plade-forkert-0.bankopladeformat \
    tests/plade-forkert-1.bankopladeformat \
    tests/plade-forkert-2.bankopladeformat | \
    ./bankopladeformat_to_futhark_input.py | \
    ./check | \
    cmp - <(echo "$expected")
