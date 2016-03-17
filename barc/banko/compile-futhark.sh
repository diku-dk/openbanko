#!/bin/sh

compiler="$1"

if ! [ "$compiler" ]; then
    compiler=futhark-c
fi

set -e
cd "$(dirname "$0")"

../dist/build/barc/barc --gotta-go-fast banko.barc > check-futhark.fut

"$compiler" check-futhark.fut
