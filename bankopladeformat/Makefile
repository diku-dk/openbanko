CC=cc
CFLAGS=-O2 -std=c99 -Wall -Wextra -pedantic -Wno-unused-function

all: bankopladeformat bankocmp bankojoin

bankopladeformat: bankopladeformat.c bankopladeformat.h
	$(CC) $(CFLAGS) -o $@ $<

bankocmp: bankocmp.c bankopladeformat.h
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f bankopladeformat bankocmp bankojoin
