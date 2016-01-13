.PHONY: clean

#CC=cc
#CFLAGS=-O2 -std=c99 -Wall -Wextra -pedantic -Wno-unused-function -Wno-self-assign -I../

all: bankopladeformat2html bankopladeformat2tex

bankopladeformat2html: bankopladeformat2html.go
	GOPATH=`pwd` go build bankopladeformat2html.go

bankopladeformat2tex: bankopladeformat2tex.go
	GOPATH=`pwd` go build bankopladeformat2tex.go

clean:
	rm bankopladeformat2html
	rm bankopladeformat2tex
