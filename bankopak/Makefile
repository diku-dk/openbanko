CC=cc
CFLAGS=-O2 -std=c99 -Wall -Wextra -pedantic -I../
ALGORITHMS=algorithm_0.h algorithm_1.h algorithm_2.h algorithm_3.h algorithm_4.h

bankopak: bankopak.c $(ALGORITHMS) bitio.h
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm bankopak
