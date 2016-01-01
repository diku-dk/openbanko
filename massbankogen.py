#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Lav en hel masse bankoplader!
"""

import itertools
import random
import sys
from sys import stdout

# Slam begynder her.

def generate_bankoplade():
    poss = []
    yxs = {}
    for y in range(3):
        yxs[y] = []
    for x in range(9):
        ys = []
        for i in range(3):
            if len(yxs[i]) < 5:
                ys.append(i)
        random.shuffle(ys)
        y = ys[0]
        poss.append((x, y))
        yxs[y].append(x)
    for y in range(3):
        xs = range(9)
        ts = yxs[y]
        for t in ts:
            xs.remove(t)
        random.shuffle(xs)
        xs = xs[:5 - len(ts)]
        for x in xs:
            poss.append((x, y))

    banko = [None for _ in range(9)]
    xs = range(9)
    random.shuffle(xs)
    for x in xs:
        col = []
        banko[x] = col
        nns = 0
        for y in range(3):
            if (x, y) in poss:
                col.append(True)
                nns += 1
            else:
                col.append(None)
        ns = range(9 if x < 8 else 10)
        random.shuffle(ns)
        ns = ns[:nns]
        ns.sort()
        j = 0
        for i in range(3):
            if col[i]:
                col[i] = ns[j] + 1 + 10 * x
                j += 1
    return banko

def print_n_plader(n):
    stdout.write('[\n')
    for i in range(n):
        plade = generate_bankoplade()
        stdout.write('[')
        for row in range(3):
            stdout.write('[')
            for col in range(9):
                num = plade[col][row]
                if num:
                    stdout.write(str(num))
                else:
                    stdout.write('00')
                if col != 8:
                    stdout.write(', ')
            if row != 2:
                stdout.write('],\n')
            else:
                stdout.write(']\n')
        if i != n-1:
            stdout.write('],\n')
        else:
            stdout.write(']\n')
    stdout.write(']\n')

if __name__ == '__main__':
    print_n_plader(int(sys.argv[1]))
