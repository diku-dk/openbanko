.PHONY: clean bankopladeformat2html bankopladeformat2tex

all: bankopladeformat2html bankopladeformat2tex

bankopladeformat2html: bankopladeformat2html.go
	GOPATH=`pwd` go build bankopladeformat2html.go

bankopladeformat2tex: bankopladeformat2tex.go
	GOPATH=`pwd` go build bankopladeformat2tex.go

clean:
	rm -f bankopladeformat2html
	rm -f bankopladeformat2tex
