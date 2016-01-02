CC=cc
CFLAGS=-O2 -std=c99 -Wall -Wextra -pedantic -I../
ALGORITHMS=algorithm_0.h algorithm_1.h

bankopak: bankopak.c $(ALGORITHMS)
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm bankopak
