#!/usr/bin/env python3

import sys

print([[[int(s) for s in line.split(' ')]
        for line in board.split('\n')]
       for board in sys.stdin.read().strip().split('\n\n')])
