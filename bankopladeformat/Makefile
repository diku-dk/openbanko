CC=cc
CFLAGS=-O2 -std=c99 -Wall -Wextra -pedantic -Wno-unused-function

all: bankopladeformat bankocmp bankosplit bankoanalyse

bankopladeformat: bankopladeformat.c bankopladeformat.h
	$(CC) $(CFLAGS) -o $@ $<

bankocmp: bankocmp.c bankopladeformat.h
	$(CC) $(CFLAGS) -o $@ $<

bankosplit: bankosplit.c bankopladeformat.h
	$(CC) $(CFLAGS) -o $@ $<

bankoanalyse: bankoanalyse.c bankopladeformat.h
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f bankopladeformat bankocmp bankojoin bankosplit_test_*

TESTFILE=../boards/1000_boards.bankopladeformat

test_bankosplit: bankosplit bankocmp
	cat $(TESTFILE) | ./bankosplit 17 bankosplit_test_17 0 bankosplit_test_0 100 bankosplit_test_100 bankosplit_test_rest
	[ `wc -l bankosplit_test_17 | cut -d' ' -f1` = 68 ]
	[ `wc -l bankosplit_test_0 | cut -d' ' -f1` = 0 ]
	[ `wc -l bankosplit_test_100 | cut -d' ' -f1` = 400 ]
	[ `wc -l bankosplit_test_rest | cut -d' ' -f1` = 3532 ]
	cat bankosplit_test_17 bankosplit_test_0 bankosplit_test_100 bankosplit_test_rest | ./bankocmp /dev/stdin $(TESTFILE)
	rm bankosplit_test_17
	rm bankosplit_test_0
	rm bankosplit_test_100
	rm bankosplit_test_rest

test: test_bankosplit
