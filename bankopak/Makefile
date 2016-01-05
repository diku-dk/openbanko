CC=cc
CFLAGS=-O2 -std=c99 -Wall -Wextra -I../
ALGORITHMS=algorithm_0.h algorithm_1.h algorithm_2.h algorithm_3.h
ALGORITHMS+=algorithm_4.h algorithm_5.h
ALGORITHMS+=algorithm_6.h algorithm_6_row_mask_permutations.h

bankopak: bankopak.c $(ALGORITHMS) bitio.h
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm bankopak
