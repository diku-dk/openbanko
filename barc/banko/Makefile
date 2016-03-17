.PHONY: all test test-futhark clean

all: test

test: check
	./test.sh

check: banko.barc ../dist/build/barc/barc
	./compile.sh

test-futhark: check-futhark
	./test-futhark.sh

check-futhark: banko.barc ../dist/build/barc/barc
	./compile-futhark.sh

clean:
	rm -f check.c check check-futhark.fut check-futhark
