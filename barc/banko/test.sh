#!/bin/sh

set -e
cd "$(dirname "$0")"

correct() {
    if ! ./check < "$1"; then
        echo "$1 should return true, but returned false"
        exit 1
    fi
}

wrong() {
    if ./check < "$1"; then
        echo "$1 should return false, but returned true"
        exit 1
    fi
}

correct tests/plade-rigtig-0.bankopladeformat
correct tests/plade-rigtig-1.bankopladeformat
wrong tests/plade-forkert-0.bankopladeformat
wrong tests/plade-forkert-1.bankopladeformat
wrong tests/plade-forkert-2.bankopladeformat
