CC=cc
CFLAGS=-O2 -std=c99 -Wall -Wextra -I../ -Wno-unused-function -lpthread -ldl -lm
ALGORITHMS=algorithm_0.h algorithm_1.h algorithm_2.h algorithm_3.h
ALGORITHMS+=algorithm_4.h algorithm_5.h
ALGORITHMS+=algorithm_6.h algorithm_6_row_mask_permutations.h
ALGORITHMS+=algorithm_7.h algorithm_8.h


bankopak: bankopak.c $(ALGORITHMS) bitio.h rust_pak/libbanko_static.a
	$(CC) -o $@ $< rust_pak/libbanko_static.a $(CFLAGS)

rust_pak/libbanko_static.a:
	$(MAKE) -C rust_pak staticlib

clean:
	rm -f bankopak
	$(MAKE) -C rust_pak clean

TESTFILE=../boards/1000_boards.bankopladeformat

# MAKEFILES SUCK
test_bankopak_%: bankopak
	@bash ./test.sh $(subst test_bankopak_,,$@) $(TESTFILE)

.PHONY: test

test: ../bankopladeformat/bankocmp test_bankopak_0 test_bankopak_1 test_bankopak_2 test_bankopak_3 test_bankopak_4 test_bankopak_5 test_bankopak_6 test_bankopak_7 test_bankopak_8

../bankopladeformat/bankocmp:
	$(MAKE) -C ../bankopladeformat
