CC=cc
CFLAGS=-O2 -std=c99 -Wall -Wextra -I../
ALGORITHMS=algorithm_0.h algorithm_1.h algorithm_2.h algorithm_3.h algorithm_4.h algorithm_5.h

bankopak: bankopak.c $(ALGORITHMS) bitio.h
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm bankopak

TESTFILE=../boards/3_boards.bankopladeformat

# MAKEFILES SUCK
test_bankopak_%: bankopak
	@echo "Testing algorithm $(subst test_bankopak_,,$@)";
	@cat $(TESTFILE) | \
	  ./bankopak -c $(subst test_bankopak_,,$@) | ./bankopak -d $(subst test_bankopak_,,$@) | \
	  ../bankopladeformat/bankocmp /dev/stdin $(TESTFILE); \

test: test_bankopak_0 test_bankopak_1 test_bankopak_2 test_bankopak_3 test_bankopak_4 test_bankopak_5
