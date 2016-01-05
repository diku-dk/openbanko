#!/bin/bash

set -e

make -C bankopladeformat
make -C bankopak

versions='1 10 1000 1000000'

gen_bankopladeformat() {
    for i in $versions; do
        echo gen_bankopladeformat $i
        ./massbankogen.py $i > boards/plader_$i.bankopladeformat
    done
}

gen_bankopak() {
    for i in $versions; do
        for j in $(seq 0 6); do
            echo gen_bankopak $i $j
            ./bankopak/bankopak -c $j < boards/plader_$i.bankopladeformat > boards/plader_$i.bankopak$j
        done
    done
}

cmp_bankopak() {
    for i in $versions; do
        for j in $(seq 0 6); do
            echo cmp_bankopak $i $j
            ./bankopladeformat/bankocmp boards/plader_$i.bankopladeformat <(./bankopak/bankopak -d $j < boards/plader_$i.bankopak$j)
        done
    done
}

gen_bankopladeformat
gen_bankopak
cmp_bankopak
