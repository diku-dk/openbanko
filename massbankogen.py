#!/usr/bin/env python3

import random
import sys


def shufrange(n):
    '''Same as range, but in shuffled order.'''
    ns = list(range(n))
    random.shuffle(ns)
    return iter(ns)

def gen_banko():
    # Generate random positions in a way that ensures that each row has 5 marked
    # positions.
    rows = []
    column_sizes = [0 for i in range(9)]
    for i in range(3):
        bs = [False for j in range(9)]
        for p in list(shufrange(9))[:5]:
            bs[p] = True
            column_sizes[p] += 1
        rows.append(bs)
    
    # Fix the positions if necessary, i.e. if a column has zero marked
    # positions, then mark one of the column's positions by unmarking a position
    # on the same row in a different column with multiple marked positions.
    # Make sure to throw in randomness everywhere.
    for i in shufrange(9):
        if column_sizes[i] == 0:
            for j in shufrange(9):
                if column_sizes[j] > 1:
                    column_sizes[j] -= 1
                    for k in shufrange(3):
                        if rows[k][j]:
                            rows[k][j] = False
                            rows[k][i] = True
                            break
                    column_sizes[i] = 1
                    break

    # Generate random numbers and Fill them into the marked positions.
    for x in range(9):
        if x == 0:
            min_n = 1
            max_n = 9
        elif x == 8:
            min_n = 80
            max_n = 90
        else:
            min_n = x * 10
            max_n = min_n + 9
        n_nums = column_sizes[x]
        ns = list(range(min_n, max_n + 1))
        random.shuffle(ns)
        ns = ns[:3]
        ns.sort()
        i = 0
        for y in range(3):
            if rows[y][x]:
                rows[y][x] = ns[i]
                i += 1
    return rows

def format_banko(rows):
    return '\n'.join(' '.join('{:02d}'.format(num) for num in row)
                       for row in rows) + '\n'

def print_bankos(n):
    for i in range(n - 1):
        print(format_banko(gen_banko()))
    print(format_banko(gen_banko()))

if __name__ == '__main__':
    try:
        n = int(sys.argv[1])
    except Exception:
        n = 1

    print_bankos(n)
