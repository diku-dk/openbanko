#!/usr/bin/env python3

# Silly.

import random
import subprocess

def random_board_values():
    return b' '.join(bytes(random.randint(0, 127)) for _ in range(27))

def check_board(board):
    proc = subprocess.Popen(['./check'], stdin=subprocess.PIPE)
    proc.communicate(input=board)
    return (proc.returncode == 0)

def gen_board():
    while True:
        board = random_board_values()
        if check_board(board):
            print(board)
            break

gen_board()
