CFLAGS=-O2 -std=c99 -Wall -Wextra -I../ -Wno-unused-function -lm -lOpenCL
FUT_CFLAGS=-O2 -std=c99

futbankosim: futbankosim.c futbanko.o
	gcc $^ -o $@ $(CFLAGS)

futbanko.o: futbanko.c
	gcc $(FUT_CFLAGS) -c $^

futbanko.c: futbanko.fut lib
	futhark opencl --library $<

lib:
	futhark pkg sync

clean:
	rm -f futbankosim futbanko.c futbanko.o futbanko.h
