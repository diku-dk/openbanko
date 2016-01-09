.PHONY: clean

CC=cc
CFLAGS=-O2 -std=c99 -Wall -Wextra -pedantic -Wno-unused-function -Wno-self-assign -I../

all: bankopladeformat2html bankopladeformat2tex

bankopladeformat2html: bankopladeformat2html.go
	go build bankopladeformat2html.go

bankopladeformat2tex: bankopladeformat2tex.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm bankopladeformat2html
	rm bankopladeformat2tex
